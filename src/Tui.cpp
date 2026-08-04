/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#include "Tui.h"

#include <atomic>
#include <chrono>
#include <condition_variable>
#include <fcntl.h>
#include <locale.h>
#include <math.h>
#include <mutex>
#include <termios.h>
#include <thread>
#include <unistd.h>
#include <vector>

#include <ncursesw/ncurses.h>

// ncurses.h defines a horde of short lowercase macros (timeout, box, ...) that
// collide with method names in rct/*.h. Undef the ones that actually clash so
// the rct headers below compile, then hand-call the ncurses functions through
// their real names.
#undef timeout
#undef box
#undef erase
#undef move

#include "Project.h"
#include "Server.h"

#include <rct/Path.h>
#include <rct/String.h>

namespace {

struct ProjectSnapshot
{
    Path key;
    String name;
    int done    = 0;
    int total   = 0;
    bool active = false;
    String lastFile;
    std::chrono::steady_clock::time_point lastFileExpiry {};
};

constexpr auto kLastFileDuration = std::chrono::seconds(2);

std::mutex sMutex;
std::atomic<bool> sEnabled { false };

FILE *sTtyFile       = nullptr;
SCREEN *sScreen      = nullptr;
WINDOW *sMessagesWin = nullptr;
WINDOW *sBarsWin     = nullptr;
int sSavedStdout     = -1;
int sSavedStderr     = -1;
int sPipeRead        = -1;
int sPipeWrite       = -1;
struct termios sSavedTermios;
bool sHaveSavedTermios = false;
std::thread sReaderThread;
std::atomic<bool> sReaderStop { false };
std::thread sTickerThread;
std::atomic<bool> sTickerStop { false };
std::condition_variable sTickerCv;
std::mutex sTickerMutex;

std::vector<ProjectSnapshot> sSnapshots;
std::vector<String> sMessages;
constexpr int kMaxMessages = 5000;
constexpr int kBarWidth    = 30;

void ensureCurrentLineExists()
{
    if (sMessages.empty())
        sMessages.push_back(String());
}

void appendMessageBytes(const char *data, size_t len)
{
    ensureCurrentLineExists();
    for (size_t i = 0; i < len; ++i) {
        const char c = data[i];
        if (c == '\r')
            continue;
        if (c == '\n') {
            sMessages.push_back(String());
            continue;
        }
        sMessages.back().append(c);
    }
    while (sMessages.size() > kMaxMessages) {
        sMessages.erase(sMessages.begin());
    }
}

void drawBars()
{
    if (!sBarsWin)
        return;
    werase(sBarsWin);

    size_t nameWidth = 0;
    for (const auto &s : sSnapshots) {
        nameWidth = std::max(nameWidth, s.name.size());
    }

    int winRows = 0, winCols = 0;
    getmaxyx(sBarsWin, winRows, winCols);
    (void)winRows;

    int row = 0;
    for (const auto &s : sSnapshots) {
        const int pct         = s.total > 0 ? static_cast<int>(round((double(s.done) / double(s.total)) * 100.0)) : 0;
        const int filledCells = s.total > 0 ? (s.done * kBarWidth) / s.total : 0;

        wmove(sBarsWin, row, 0);
        wattron(sBarsWin, A_BOLD);
        wprintw(sBarsWin, "%-*s ", static_cast<int>(nameWidth), s.name.constData());
        wattroff(sBarsWin, A_BOLD);
        waddch(sBarsWin, '[');
        for (int i = 0; i < filledCells; ++i)
            waddwstr(sBarsWin, L"\u2588");
        for (int i = filledCells; i < kBarWidth; ++i)
            waddwstr(sBarsWin, L"\u2591");
        waddch(sBarsWin, ']');
        wprintw(sBarsWin, " %3d%% (%d/%d)", pct, s.done, s.total);

        if (!s.lastFile.empty() && std::chrono::steady_clock::now() < s.lastFileExpiry) {
            wprintw(sBarsWin, " %s", s.lastFile.constData());
        }

        if (winCols > 0) {
            int y, x;
            getyx(sBarsWin, y, x);
            (void)y;
            if (x < winCols)
                wclrtoeol(sBarsWin);
        }
        ++row;
    }
    wnoutrefresh(sBarsWin);
}

void drawMessages()
{
    if (!sMessagesWin)
        return;
    werase(sMessagesWin);

    int rows = 0, cols = 0;
    getmaxyx(sMessagesWin, rows, cols);
    if (rows <= 1 || cols <= 0) {
        wnoutrefresh(sMessagesWin);
        return;
    }

    const int viewRows = rows - 1;
    const size_t total = sMessages.size();
    const size_t start = total > static_cast<size_t>(viewRows) ? total - viewRows : 0;

    wattron(sMessagesWin, A_REVERSE);
    wmove(sMessagesWin, 0, 0);
    String header = String::format<128>(" rdm log  (%zu lines) ", total);
    wprintw(sMessagesWin, "%-*s", cols, header.constData());
    wattroff(sMessagesWin, A_REVERSE);

    int r = 1;
    for (size_t i = start; i < total && r < rows; ++i, ++r) {
        wmove(sMessagesWin, r, 0);
        const String &line = sMessages[i];
        const int drawLen  = std::min<int>(cols, static_cast<int>(line.size()));
        waddnstr(sMessagesWin, line.constData(), drawLen);
    }
    wnoutrefresh(sMessagesWin);
}

void relayoutLocked()
{
    if (!sScreen)
        return;

    int rows, cols;
    getmaxyx(stdscr, rows, cols);

    const int barsRows = std::max<int>(1, static_cast<int>(sSnapshots.size()));
    const int msgRows  = std::max<int>(1, rows - barsRows - 1);

    if (sMessagesWin) {
        delwin(sMessagesWin);
        sMessagesWin = nullptr;
    }
    if (sBarsWin) {
        delwin(sBarsWin);
        sBarsWin = nullptr;
    }

    sMessagesWin = newwin(msgRows, cols, 0, 0);
    sBarsWin     = newwin(barsRows + 1, cols, msgRows, 0);
    if (sMessagesWin)
        scrollok(sMessagesWin, FALSE);
    if (sBarsWin)
        scrollok(sBarsWin, FALSE);
}

void redrawLocked()
{
    if (!sScreen)
        return;
    relayoutLocked();
    drawMessages();
    drawBars();
    doupdate();
}

void tickerLoop()
{
    while (!sTickerStop.load()) {
        std::unique_lock<std::mutex> tick(sTickerMutex);
        sTickerCv.wait_for(tick, std::chrono::milliseconds(250));
        if (sTickerStop.load())
            break;
        tick.unlock();
        std::lock_guard<std::mutex> lock(sMutex);
        const auto now  = std::chrono::steady_clock::now();
        bool needRedraw = false;
        for (auto &s : sSnapshots) {
            if (!s.lastFile.empty() && now >= s.lastFileExpiry) {
                s.lastFile.clear();
                needRedraw = true;
            }
        }
        if (needRedraw && sScreen) {
            drawMessages();
            drawBars();
            doupdate();
        }
    }
}

void readerLoop()
{
    char buf[4096];
    while (!sReaderStop.load()) {
        const ssize_t n = ::read(sPipeRead, buf, sizeof(buf));
        if (n <= 0) {
            if (n < 0 && (errno == EINTR))
                continue;
            break;
        }
        {
            std::lock_guard<std::mutex> lock(sMutex);
            appendMessageBytes(buf, static_cast<size_t>(n));
            if (sScreen) {
                drawMessages();
                drawBars();
                doupdate();
            }
        }
    }
}

} // namespace

namespace Tui {

bool enabled()
{
    return sEnabled.load();
}

bool enable()
{
    std::lock_guard<std::mutex> lock(sMutex);
    if (sEnabled.load())
        return true;

    if (!::isatty(STDOUT_FILENO))
        return false;

    sTtyFile = ::fopen("/dev/tty", "w+");
    if (!sTtyFile)
        return false;

    sHaveSavedTermios = (::tcgetattr(::fileno(sTtyFile), &sSavedTermios) == 0);

    setlocale(LC_ALL, "");

    int pipefd[2];
    if (::pipe(pipefd) != 0) {
        fclose(sTtyFile);
        sTtyFile = nullptr;
        return false;
    }
    ::fcntl(pipefd[0], F_SETFD, FD_CLOEXEC);
    ::fcntl(pipefd[1], F_SETFD, FD_CLOEXEC);

    sPipeRead  = pipefd[0];
    sPipeWrite = pipefd[1];

    sSavedStdout = ::dup(STDOUT_FILENO);
    sSavedStderr = ::dup(STDERR_FILENO);
    if (sSavedStdout < 0 || sSavedStderr < 0) {
        goto fail;
    }
    if (::dup2(sPipeWrite, STDOUT_FILENO) < 0)
        goto fail;
    if (::dup2(sPipeWrite, STDERR_FILENO) < 0)
        goto fail;

    ::setvbuf(stdout, nullptr, _IOLBF, 0);
    ::setvbuf(stderr, nullptr, _IOLBF, 0);

    sScreen = ::newterm(nullptr, sTtyFile, sTtyFile);
    if (!sScreen)
        goto fail;
    ::set_term(sScreen);
    ::cbreak();
    ::noecho();
    ::curs_set(0);
    ::nodelay(stdscr, TRUE);
    ::keypad(stdscr, TRUE);
    ::clear();
    ::refresh();

    sEnabled.store(true);
    sReaderStop.store(false);
    sReaderThread = std::thread(readerLoop);
    sTickerStop.store(false);
    sTickerThread = std::thread(tickerLoop);

    relayoutLocked();
    drawMessages();
    drawBars();
    doupdate();
    return true;

fail:
    if (sSavedStdout >= 0) {
        ::dup2(sSavedStdout, STDOUT_FILENO);
        ::close(sSavedStdout);
        sSavedStdout = -1;
    }
    if (sSavedStderr >= 0) {
        ::dup2(sSavedStderr, STDERR_FILENO);
        ::close(sSavedStderr);
        sSavedStderr = -1;
    }
    if (sPipeRead >= 0) {
        ::close(sPipeRead);
        sPipeRead = -1;
    }
    if (sPipeWrite >= 0) {
        ::close(sPipeWrite);
        sPipeWrite = -1;
    }
    if (sTtyFile) {
        ::fclose(sTtyFile);
        sTtyFile = nullptr;
    }
    return false;
}

void disable()
{
    if (!sEnabled.exchange(false))
        return;

    sReaderStop.store(true);
    sTickerStop.store(true);
    sTickerCv.notify_all();
    if (sTickerThread.joinable())
        sTickerThread.join();

    if (sPipeWrite >= 0) {
        ::close(sPipeWrite);
        sPipeWrite = -1;
    }

    if (sReaderThread.joinable())
        sReaderThread.join();

    if (sPipeRead >= 0) {
        ::close(sPipeRead);
        sPipeRead = -1;
    }

    std::lock_guard<std::mutex> lock(sMutex);

    if (sSavedStdout >= 0) {
        ::dup2(sSavedStdout, STDOUT_FILENO);
        ::close(sSavedStdout);
        sSavedStdout = -1;
    }
    if (sSavedStderr >= 0) {
        ::dup2(sSavedStderr, STDERR_FILENO);
        ::close(sSavedStderr);
        sSavedStderr = -1;
    }

    if (sMessagesWin) {
        delwin(sMessagesWin);
        sMessagesWin = nullptr;
    }
    if (sBarsWin) {
        delwin(sBarsWin);
        sBarsWin = nullptr;
    }
    if (sScreen) {
        ::set_term(sScreen);
        ::curs_set(1);
        ::echo();
        ::nocbreak();
        ::endwin();
        ::delscreen(sScreen);
        sScreen = nullptr;
    }
    if (sTtyFile) {
        const int ttyFd = ::fileno(sTtyFile);
        if (sHaveSavedTermios && ttyFd >= 0) {
            ::tcsetattr(ttyFd, TCSANOW, &sSavedTermios);
        }
        static const char kRestore[] = "\033[?25h\033[0m\r";
        (void)!::write(ttyFd, kRestore, sizeof(kRestore) - 1);
        ::fclose(sTtyFile);
        sTtyFile = nullptr;
    }
    sHaveSavedTermios = false;
}

void asyncSafeRestore()
{
    if (!sEnabled.load())
        return;
    const int fd = ::open("/dev/tty", O_WRONLY | O_NOCTTY);
    if (fd >= 0) {
        if (sHaveSavedTermios) {
            ::tcsetattr(fd, TCSANOW, &sSavedTermios);
        }
        static const char kRestore[] = "\033[?25h\033[0m\r\n";
        (void)!::write(fd, kRestore, sizeof(kRestore) - 1);
        ::close(fd);
    }
}

void update(const std::shared_ptr<Project> &project, int done, int total, const char *fileName)
{
    if (!sEnabled.load() || !project)
        return;

    std::lock_guard<std::mutex> lock(sMutex);

    const Path key        = project->path();
    ProjectSnapshot *slot = nullptr;
    for (auto &s : sSnapshots) {
        if (s.key == key) {
            slot = &s;
            break;
        }
    }
    if (!slot) {
        sSnapshots.push_back(ProjectSnapshot());
        slot      = &sSnapshots.back();
        slot->key = key;
    }
    slot->name = project->displayName();

    // First real progress event replaces the seeded 100% snapshot so the new
    // indexing round is visible from 0%. After that, keep the most-completed
    // snapshot so finished projects stay pinned at 100%.
    if (!slot->active) {
        slot->active = true;
        slot->total  = total;
        slot->done   = done;
    } else if (total > slot->total) {
        slot->total = total;
        slot->done  = done;
    } else if (total == slot->total && done > slot->done) {
        slot->done = done;
    }

    if (fileName && *fileName) {
        slot->lastFile       = fileName;
        slot->lastFileExpiry = std::chrono::steady_clock::now() + kLastFileDuration;
        sTickerCv.notify_all();
    }

    redrawLocked();
}

void registerProject(const std::shared_ptr<Project> &project, int sourceCount)
{
    if (!sEnabled.load() || !project)
        return;

    std::lock_guard<std::mutex> lock(sMutex);

    const Path key = project->path();
    for (const auto &s : sSnapshots) {
        if (s.key == key)
            return;
    }
    ProjectSnapshot snap;
    snap.key    = key;
    snap.name   = project->displayName();
    snap.total  = std::max(sourceCount, 0);
    snap.done   = snap.total;
    snap.active = false;
    sSnapshots.push_back(std::move(snap));

    redrawLocked();
}

} // namespace Tui
