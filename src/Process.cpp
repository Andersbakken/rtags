#include "Process.h"
#include "EventLoop.h"
#include "Log.h"
#include "Rdm.h"
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

void Process::setCwd(const Path& cwd)
{
    mCwd = cwd;
}

void Process::start(const ByteArray& command,
                    const List<ByteArray>& arguments)
{
    start(command, arguments, List<ByteArray>());
}

Path Process::findCommand(const ByteArray& command)
{
    if (command.isEmpty() || command.at(0) == '/')
        return command;

    const char* path = getenv("PATH");
    if (!path)
        return Path();
    bool ok;
    const List<ByteArray> paths = ByteArray(path).split(':');
    for (List<ByteArray>::const_iterator it = paths.begin(); it != paths.end(); ++it) {
        const Path ret = Path::resolved(command, *it, &ok);
        if (ok && !access(ret.nullTerminated(), R_OK | X_OK))
            return ret;
    }
    return Path();
}

void Process::start(const ByteArray& command,
                    const List<ByteArray>& arguments,
                    const List<ByteArray>& environ)
{
    ByteArray cmd = findCommand(command);
    if (cmd.isEmpty())
        return;

    int err;

    eintrwrap(err, ::pipe(mStdIn));
    eintrwrap(err, ::pipe(mStdOut));
    eintrwrap(err, ::pipe(mStdErr));

    const char* args[arguments.size() + 2];
    args[arguments.size() + 1] = 0;
    args[0] = cmd.nullTerminated();
    int pos = 1;
    for(List<ByteArray>::const_iterator it = arguments.begin(); it != arguments.end(); ++it) {
        args[pos] = it->nullTerminated();
        //printf("arg: '%s'\n", args[pos]);
        ++pos;
    }
    const char* env[environ.size() + 1];
    env[environ.size()] = 0;
    pos = 0;
    //printf("fork, about to exec '%s'\n", cmd.nullTerminated());
    for(List<ByteArray>::const_iterator it = environ.begin(); it != environ.end(); ++it) {
        env[pos] = it->nullTerminated();
        //printf("env: '%s'\n", env[pos]);
        ++pos;
    }

    mPid = ::fork();
    if (mPid == -1) {
        printf("fork, something horrible has happened %d\n", errno);
        // bail out
        eintrwrap(err, ::close(mStdIn[1]));
        eintrwrap(err, ::close(mStdIn[0]));
        eintrwrap(err, ::close(mStdOut[1]));
        eintrwrap(err, ::close(mStdOut[0]));
        eintrwrap(err, ::close(mStdErr[1]));
        eintrwrap(err, ::close(mStdErr[0]));
        return;
    } else if (mPid == 0) {
        //printf("fork, in child\n");
        // child, should do some error checking here really
        eintrwrap(err, ::close(mStdIn[1]));
        eintrwrap(err, ::close(mStdOut[0]));
        eintrwrap(err, ::close(mStdErr[0]));

        eintrwrap(err, ::close(STDIN_FILENO));
        eintrwrap(err, ::close(STDOUT_FILENO));
        eintrwrap(err, ::close(STDERR_FILENO));

        eintrwrap(err, ::dup2(mStdIn[0], STDIN_FILENO));
        eintrwrap(err, ::close(mStdIn[0]));
        eintrwrap(err, ::dup2(mStdOut[1], STDOUT_FILENO));
        eintrwrap(err, ::close(mStdOut[1]));
        eintrwrap(err, ::dup2(mStdErr[1], STDERR_FILENO));
        eintrwrap(err, ::close(mStdErr[1]));

        if (!mCwd.isEmpty())
            eintrwrap(err, ::chdir(mCwd.nullTerminated()));
        const int ret = ::execve(cmd.nullTerminated(), const_cast<char* const*>(args), const_cast<char* const*>(env));
        ::_exit(1);
        (void)ret;
        //printf("fork, exec seemingly failed %d, %d %s\n", ret, errno, strerror(errno));
    } else {
        // parent
        eintrwrap(err, ::close(mStdIn[0]));
        eintrwrap(err, ::close(mStdOut[1]));
        eintrwrap(err, ::close(mStdErr[1]));

        //printf("fork, in parent\n");

        int flags;
        eintrwrap(flags, fcntl(mStdIn[1], F_GETFL, 0));
        eintrwrap(flags, fcntl(mStdIn[1], F_SETFL, flags | O_NONBLOCK));
        eintrwrap(flags, fcntl(mStdOut[0], F_GETFL, 0));
        eintrwrap(flags, fcntl(mStdOut[0], F_SETFL, flags | O_NONBLOCK));
        eintrwrap(flags, fcntl(mStdErr[0], F_GETFL, 0));
        eintrwrap(flags, fcntl(mStdErr[0], F_SETFL, flags | O_NONBLOCK));

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
    int err;
    eintrwrap(err, ::close(mStdIn[1]));
    mStdIn[1] = -1;
}

void Process::closeStdOut()
{
    if (mStdOut[0] == -1)
        return;

    EventLoop::instance()->removeFileDescriptor(mStdOut[0]);
    int err;
    eintrwrap(err, ::close(mStdOut[0]));
    mStdOut[0] = -1;
}

void Process::closeStdErr()
{
    if (mStdErr[0] == -1)
        return;

    EventLoop::instance()->removeFileDescriptor(mStdErr[0]);
    int err;
    eintrwrap(err, ::close(mStdErr[0]));
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

    //static int ting = 0;
    //printf("Process::handleInput (cnt=%d)\n", ++ting);
    for (;;) {
        if (mStdInBuffer.empty())
            return;

        //printf("Process::handleInput in loop\n");
        int w, want;
        const ByteArray& front = mStdInBuffer.front();
        if (mStdInIndex) {
            want = front.size() - mStdInIndex;
            eintrwrap(w, ::write(fd, front.mid(mStdInIndex).constData(), want));
        } else {
            want = front.size();
            eintrwrap(w, ::write(fd, front.constData(), want));
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
    //printf("Process::handleOutput %d\n", fd);
    enum { BufSize = 1024, MaxSize = (1024 * 1024 * 16) };
    char buf[BufSize];
    int total = 0;
    bool term = false;
    for (;;) {
        int r;
        eintrwrap(r, ::read(fd, buf, BufSize));
        if (r == -1) {
            //printf("Process::handleOutput %d returning -1, errno %d %s\n", fd, errno, strerror(errno));
            break;
        } else if (r == 0) { // Assume process terminated
            //printf("Process::handleOutput %d returning 0\n", fd);
            term = true;
            break;
        } else {
            //printf("Process::handleOutput in loop %d\n", fd);
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

    int err;
    eintrwrap(err, ::waitpid(mPid, &mReturn, WNOHANG));
    mPid = -1;

    mFinished();
}

void Process::stop()
{
    if (mPid == -1)
        return;

    ::kill(mPid, SIGTERM);
    int err;
    eintrwrap(err, ::waitpid(mPid, &mReturn, 0));
}

List<ByteArray> Process::environment()
{
    extern char** environ;
    char** cur = environ;
    List<ByteArray> env;
    while (*cur) {
        env.push_back(*cur);
        ++cur;
    }
    return env;
}
