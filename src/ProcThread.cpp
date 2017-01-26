/* This file is part of RTags (http://rtags.net).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include "ProcThread.h"
#include <rct/StopWatch.h>
#include "RTags.h"
#ifdef RTAGS_HAS_PROC
#include <sys/types.h>
#include <dirent.h>
#endif

ProcThread::ProcThread(int interval)
    : Thread(), mInterval(interval)
{
#ifdef RTAGS_HAS_PROC
    mPath = "/proc/";
#endif
}

ProcThread::~ProcThread()
{
}

void ProcThread::run()
{
    while (true) {
        {
            std::unique_lock<std::mutex> lock(mMutex);
            if (!mInterval) {
                break;
            }
            if (mCond.wait_for(lock, std::chrono::milliseconds(mInterval)) != std::cv_status::timeout) {
                break;
            }
        }
        readProc();
    }
}

void ProcThread::stop()
{
    {
        std::unique_lock<std::mutex> lock(mMutex);
        mInterval = 0;
        mCond.notify_one();
    }
    join();
}

struct ParseNode {
    char cmdLine[16384];
    char cwd[PATH_MAX];
    List<String> environ;

};
void ProcThread::readProc()
{
    for (auto &pair : mNodes) {
        pair.second = true;
    }
#ifdef RTAGS_HAS_PROC
    DIR *dir = opendir("/proc/");
    int found = 0;
    StopWatch sw;
    List<ParseNode> nodes(1);
    while (const dirent *p = readdir(dir)) {
#if defined(_DIRENT_HAVE_D_TYPE) && defined(_BSD_SOURCE)
        if (p->d_type != DT_DIR)
            continue;
#endif
        const int pid = atoi(p->d_name);
        if (!pid)
            continue;
        auto it = mNodes.find(pid);
        if (it != mNodes.end()) {
            it->second = false;
            continue;
        } else {
            mNodes[pid] = false;
        }

        char file[PATH_MAX]; // should be enough for everyone
        snprintf(file, sizeof(file), "/proc/%s/cmdline", p->d_name);
        FILE *f = fopen(file, "r");
        if (!f) {
            // error("Can't open %s for reading", file);
            continue;
        }

        auto &node = nodes.back();

        size_t read = fread(node.cmdLine, 1, sizeof(node.cmdLine), f);
        fclose(f);
        if (!read) {
            // error("Can't read from %s", file);
            continue;
        }

        if (!strncmp(node.cmdLine, "/bin/bash", 9) || !strncmp(node.cmdLine, "/bin/sh", 7))
            continue;

        while (read > 0 && !node.cmdLine[read-1])
            --read; // skip trailing zeroes
        const char *prev = node.cmdLine;
        enum {
            Dunno,
            Yay,
            Nay
        } hasSource = Dunno;
        for (size_t i=0; i<read; ++i) {
            switch (node.cmdLine[i]) {
            case '\n':
            case '\0':
            case ' ':
                if (prev == node.cmdLine) {
                    node.cmdLine[i] = '\0';
                    // error() << "Considering" << node.cmdLine;
                    if (strstr(node.cmdLine, "cc1plus") || strstr(node.cmdLine, "/rc")) {
                        hasSource = Nay;
                    }
                }
                node.cmdLine[i] = ' ';
                if (i && hasSource == Dunno) {
                    for (const char *ext = &node.cmdLine[i - 1]; ext > prev; --ext) {
                        if (*ext == '.') {
                            char extBuf[16];
                            const size_t size = i - 1 - (ext - node.cmdLine);
                            if (size < sizeof(extBuf)) {
                                memcpy(extBuf, ext + 1, size);
                                extBuf[size] = '\0';
                                if (Path::isSource(extBuf)) {
                                    hasSource = Yay;
                                }
                            }
                            break;
                        }
                    }
                }
                prev = &node.cmdLine[i];
            default:
                break;
            }
        }
        if (hasSource != Yay)
            continue;

        // error() << "GOT SOURCE" << node.cmdLine;

        snprintf(file, sizeof(file), "/proc/%s/cwd", p->d_name);
        const int w = readlink(file, node.cwd, sizeof(node.cwd) - 2);
        if (w <= 0) {
            // error("Can't follow link from %s", file);
            continue;
        }

        if (node.cwd[w - 1] != '/') {
            node.cwd[w] = '/';
            node.cwd[w + 1] = '\0';
        } else {
            node.cwd[w] = '\0';
        }

        snprintf(file, sizeof(file), "/proc/%s/environ", p->d_name);
        f = fopen(file, "r");
        if (!f) {
            // error("Can't open file %s", file);
            continue;
        }

        char env[16384];
        read = fread(env, 1, sizeof(env), f);
        fclose(f);
        if (!read) {
            // error("Can't read from from %s", file);
            continue;
        }

        while (read > 0 && !node.cmdLine[read-1])
            --read; // skip trailing zeroes
        size_t last = 0;
        for (size_t i=0; i<read; ++i) {
            switch (env[i]) {
            case '\n':
            case '\0':
                node.environ.append(String(env + last, i - last));
                last = i + 1;
                break;
            default:
                break;
            }
        }
        if (read > last) {
            node.environ.append(String(env + last, read - last));
        }
        nodes.append(ParseNode());
        // error() << "GONNA PARSE" << node.cmdLine;
        // if (!sources.isEmpty()) {
        //     error() << "GOT SOURCES" << sources[0].sourceFile();
        // }
        ++found;
    }
    SourceCache cache;
    for (size_t i=0; i<nodes.size() - 1; ++i) {
        const auto &node = nodes.at(i);
        List<Path> paths;
        SourceList sources = Source::parse(node.cmdLine, node.cwd, node.environ, &paths, &cache);
        if (sources.size()) {
            error() << "GOT SOURCES" << node.cmdLine;
        }
        // error() << "GOT SOURCES" << sources.size() << "from" << node.cmdLine
        //         << paths;
    }

    Hash<int, bool>::iterator it = mNodes.begin();
    while (it != mNodes.end()) {
        if (it->second) {
            mNodes.erase(it++);
        } else {
            ++it;
        }
    }
    // printf("%d took %llu\n", found, sw.elapsed());
    closedir(dir);
#endif
}
