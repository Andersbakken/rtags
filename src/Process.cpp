#include "Process.h"
#include "EventLoop.h"
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>

Process::Process()
    : mPid(-1), mReturn(0), mStdInIndex(0), mStdOutIndex(0), mStdErrIndex(0)
{
    mStdIn[0] = mStdIn[1] = -1;
    mStdOut[0] = mStdOut[1] = -1;
    mStdErr[0] = mStdErr[1] = -1;
}

Process::~Process()
{
    if (mStdIn[0] != -1) {
        // try to finish off any pending writes
        handleInput(mStdIn[1]);
    }

    closeStdIn();
    closeStdOut();
    closeStdErr();
}

void Process::setCwd(const ByteArray& cwd)
{
    mCwd = cwd;
}

void Process::start(const ByteArray& command,
                    const std::list<ByteArray>& arguments)
{
    start(command, arguments, std::list<ByteArray>());
}

void Process::start(const ByteArray& command,
                    const std::list<ByteArray>& arguments,
                    const std::list<ByteArray>& environ)
{
    ::pipe(mStdIn);
    ::pipe(mStdOut);
    ::pipe(mStdErr);

    mPid = ::fork();
    if (mPid == -1) {
        printf("fork, something horrible has happened %d\n", errno);
        // bail out
        ::close(mStdIn[1]);
        ::close(mStdIn[0]);
        ::close(mStdOut[1]);
        ::close(mStdOut[0]);
        ::close(mStdErr[1]);
        ::close(mStdErr[0]);
        return;
    } else if (mPid == 0) {
        //printf("fork, in child\n");
        // child, should do some error checking here really
        ::close(mStdIn[1]);
        ::close(mStdOut[0]);
        ::close(mStdErr[0]);

        ::close(STDIN_FILENO);
        ::close(STDOUT_FILENO);
        ::close(STDERR_FILENO);

        ::dup2(mStdIn[0], STDIN_FILENO);
        ::close(mStdIn[0]);
        ::dup2(mStdOut[1], STDOUT_FILENO);
        ::close(mStdOut[1]);
        ::dup2(mStdErr[1], STDERR_FILENO);
        ::close(mStdErr[1]);

        const char* args[arguments.size() + 2];
        args[arguments.size() + 1] = 0;
        args[0] = command.nullTerminated();
        int pos = 1;
        for(std::list<ByteArray>::const_iterator it = arguments.begin(); it != arguments.end(); ++it) {
            args[pos] = it->nullTerminated();
            //printf("arg: '%s'\n", args[pos]);
            ++pos;
        }
        const char* env[environ.size() + 1];
        env[environ.size()] = 0;
        pos = 0;
        //printf("fork, about to exec '%s'\n", command.nullTerminated());
        for(std::list<ByteArray>::const_iterator it = environ.begin(); it != environ.end(); ++it) {
            env[pos] = it->nullTerminated();
            //printf("env: '%s'\n", env[pos]);
            ++pos;
        }
        if (!mCwd.isEmpty())
            ::chdir(mCwd.nullTerminated());
        const int ret = ::execvpe(command.nullTerminated(), const_cast<char* const*>(args), const_cast<char* const*>(env));
        (void)ret;
        //printf("fork, exec seemingly failed %d, %d %s\n", ret, errno, strerror(errno));
    } else {
        // parent
        ::close(mStdIn[0]);
        ::close(mStdOut[1]);
        ::close(mStdErr[1]);

        //printf("fork, in parent\n");

        int flags = fcntl(mStdIn[1], F_GETFL, 0);
        fcntl(mStdIn[1], F_SETFL, flags | O_NONBLOCK);
        flags = fcntl(mStdOut[0], F_GETFL, 0);
        fcntl(mStdOut[0], F_SETFL, flags | O_NONBLOCK);
        flags = fcntl(mStdErr[0], F_GETFL, 0);
        fcntl(mStdErr[0], F_SETFL, flags | O_NONBLOCK);

        //printf("fork, about to add fds: stdin=%d, stdout=%d, stderr=%d\n", mStdIn[1], mStdOut[0], mStdErr[0]);
        EventLoop::instance()->addFileDescriptor(mStdOut[0], EventLoop::Read, processCallback, this);
        EventLoop::instance()->addFileDescriptor(mStdErr[0], EventLoop::Read, processCallback, this);
    }
}

void Process::write(const ByteArray& data)
{
    if (!data.isEmpty() && mStdIn[1] != -1) {
        mStdInBuffer.push_back(data);
        handleInput(mStdIn[1]);
    }
}

void Process::closeStdIn()
{
    if (mStdIn[1] == -1)
        return;

    EventLoop::instance()->removeFileDescriptor(mStdIn[1]);
    ::close(mStdIn[1]);
    mStdIn[1] = -1;
}

void Process::closeStdOut()
{
    if (mStdOut[0] == -1)
        return;

    EventLoop::instance()->removeFileDescriptor(mStdOut[0]);
    ::close(mStdOut[0]);
    mStdOut[0] = -1;
}

void Process::closeStdErr()
{
    if (mStdErr[0] == -1)
        return;

    EventLoop::instance()->removeFileDescriptor(mStdErr[0]);
    ::close(mStdErr[0]);
    mStdErr[0] = -1;
}

ByteArray Process::readAllStdOut()
{
    ByteArray out;
    std::swap(mStdOutBuffer, out);
    mStdOutIndex = 0;
    return out;
}

ByteArray Process::readAllStdErr()
{
    ByteArray out;
    std::swap(mStdErrBuffer, out);
    mStdErrIndex = 0;
    return out;
}

void Process::processCallback(int fd, unsigned int flags, void* userData)
{
    Process* proc = reinterpret_cast<Process*>(userData);
    if (fd == proc->mStdIn[1])
        proc->handleInput(fd);
    else if (fd == proc->mStdOut[0])
        proc->handleOutput(fd, proc->mStdOutBuffer, proc->mStdOutIndex, proc->mReadyReadStdOut);
    else if (fd == proc->mStdErr[0])
        proc->handleOutput(fd, proc->mStdErrBuffer, proc->mStdErrIndex, proc->mReadyReadStdErr);
}

void Process::handleInput(int fd)
{
    EventLoop::instance()->removeFileDescriptor(fd);

    static int balle = 0;
    printf("Process::handleInput (cnt=%d)\n", ++balle);
    for (;;) {
        if (mStdInBuffer.empty())
            return;

        printf("Process::handleInput in loop\n");
        int w, want;
        const ByteArray& front = mStdInBuffer.front();
        if (mStdInIndex) {
            want = front.size() - mStdInIndex;
            w = ::write(fd, front.mid(mStdInIndex).constData(), want);
        } else {
            want = front.size();
            w = ::write(fd, front.constData(), want);
        }
        if (w == -1) {
            EventLoop::instance()->addFileDescriptor(fd, EventLoop::Write, processCallback, this);
            break;
        } else if (w == want) {
            mStdInBuffer.pop_front();
            mStdInIndex = 0;
        } else
            mStdInIndex += w;
    }
}

void Process::handleOutput(int fd, ByteArray& buffer, int& index, signalslot::Signal0& signal)
{
    printf("Process::handleOutput %d\n", fd);
    enum { BufSize = 1024, MaxSize = (1024 * 1024 * 16) };
    char buf[BufSize];
    int total = 0;
    bool term = false;
    for (;;) {
        int r = ::read(fd, buf, BufSize);
        if (r == -1) {
            printf("Process::handleOutput %d returning -1, errno %d %s\n", fd, errno, strerror(errno));
            break;
        } else if (r == 0) { // Assume process terminated
            printf("Process::handleOutput %d returning 0\n", fd);
            term = true;
            break;
        } else {
            printf("Process::handleOutput in loop %d\n", fd);
            //printf("data: '%s'\n", std::string(buf, r).c_str());
            int sz = buffer.size();
            if (sz + r > MaxSize) {
                if (sz + r - index > MaxSize) {
                    error("Process::handleOutput, buffer too big, dropping data");
                    buffer.clear();
                    index = sz = 0;
                } else {
                    sz = buffer.size() - index;
                    memmove(buffer.data(), buffer.data() + index, sz);
                    buffer.resize(sz);
                    index = 0;
                }
            }
            buffer.resize(sz + r);
            memcpy(buffer.data() + sz, buf, r);

            total += r;
        }
    }

    //printf("total data '%s'\n", buffer.nullTerminated());

    if (total)
        signal();
    if (term)
        handleTerminated();
}

void Process::handleTerminated()
{
    if (mPid == -1)
        return;

    mStdInBuffer.clear();
    closeStdIn();
    closeStdOut();
    closeStdErr();

    ::waitpid(mPid, &mReturn, WNOHANG);
    mPid = -1;

    mFinished();
}

void Process::stop()
{
    if (mPid == -1)
        return;

    ::kill(mPid, SIGTERM);
    ::waitpid(mPid, &mReturn, 0);
}

std::list<ByteArray> Process::environment()
{
    extern char** environ;
    char** cur = environ;
    std::list<ByteArray> env;
    while (*cur) {
        env.push_back(*cur);
        ++cur;
    }
    return env;
}
