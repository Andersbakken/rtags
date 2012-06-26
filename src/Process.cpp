#include "Process.h"
#include "EventLoop.h"
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

void Process::start(const ByteArray& command, const std::list<ByteArray>& arguments)
{
    ::pipe(mStdIn);
    ::pipe(mStdOut);
    ::pipe(mStdErr);

    mPid = ::fork();
    if (mPid == -1) {
        // bail out
        ::close(mStdIn[1]);
        ::close(mStdIn[0]);
        ::close(mStdOut[1]);
        ::close(mStdOut[0]);
        ::close(mStdErr[1]);
        ::close(mStdErr[0]);
        return;
    } else if (mPid == 0) {
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

        const char* args[arguments.size() + 1];
        args[arguments.size()] = 0;
        int pos = 0;
        for(std::list<ByteArray>::const_iterator it = arguments.begin(); it != arguments.end(); ++it) {
            args[pos] = it->nullTerminated();
            ++pos;
        }
        ::execv(command.nullTerminated(), const_cast<char* const*>(args));
    } else {
        // parent
        ::close(mStdIn[0]);
        ::close(mStdOut[1]);
        ::close(mStdErr[1]);

        int flags = fcntl(mStdIn[1], F_GETFL, 0);
        fcntl(mStdIn[1], F_SETFL, flags | O_NONBLOCK);
        flags = fcntl(mStdOut[0], F_GETFL, 0);
        fcntl(mStdOut[0], F_SETFL, flags | O_NONBLOCK);
        flags = fcntl(mStdErr[0], F_GETFL, 0);
        fcntl(mStdErr[0], F_SETFL, flags | O_NONBLOCK);

        EventLoop::instance()->addFileDescriptor(mStdIn[1], EventLoop::Write, processCallback, this);
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
    if (mStdOut[1] == -1)
        return;

    EventLoop::instance()->removeFileDescriptor(mStdOut[1]);
    ::close(mStdOut[1]);
    mStdOut[1] = -1;
}

void Process::closeStdErr()
{
    if (mStdErr[1] == -1)
        return;

    EventLoop::instance()->removeFileDescriptor(mStdErr[1]);
    ::close(mStdErr[1]);
    mStdErr[1] = -1;
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
    for (;;) {
        if (mStdInBuffer.empty())
            return;

        int w, want;
        const ByteArray& front = mStdInBuffer.front();
        if (mStdInIndex) {
            want = front.size() - mStdInIndex;
            w = ::write(fd, front.mid(mStdInIndex).constData(), want);
        } else {
            want = front.size();
            w = ::write(fd, front.constData(), want);
        }
        if (w == -1)
            break;
        if (w == want) {
            mStdInBuffer.pop_front();
            mStdInIndex = 0;
        } else
            mStdInIndex += w;
    }
}

void Process::handleOutput(int fd, ByteArray& buffer, int& index, signalslot::Signal0& signal)
{
    enum { BufSize = 1024, MaxSize = (1024 * 1024 * 16) };
    char buf[BufSize];
    int total = 0;
    bool term = false;
    for (;;) {
        int r = ::read(fd, buf, BufSize);
        if (r == -1) {
            break;
        } else if (r == 0) { // Assume process terminated
            term = true;
            break;
        } else {
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
