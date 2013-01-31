#ifndef PROCESS_H
#define PROCESS_H

#include "String.h"
#include "EventReceiver.h"
#include "Path.h"
#include "List.h"
#include "SignalSlot.h"
#include <deque>

class Process : public EventReceiver
{
public:
    Process();
    ~Process();

    void setCwd(const Path& cwd);

    bool start(const String& command, const List<String>& arguments);
    bool start(const String& command, const List<String>& arguments,
               const List<String>& environ);

    String errorString() const { return mErrorString; }

    void write(const String& data);
    void closeStdIn();

    String readAllStdOut();
    String readAllStdErr();

    bool isFinished() const { return mPid == -1; }
    int returnCode() const { return mReturn; }

    void stop();

    signalslot::Signal0& readyReadStdOut() { return mReadyReadStdOut; }
    signalslot::Signal0& readyReadStdErr() { return mReadyReadStdErr; }
    signalslot::Signal0& finished() { return mFinished; }

    static List<String> environment();

    static Path findCommand(const String& command);

protected:
    virtual void event(const Event* event);

private:
    static void processCallback(int fd, unsigned int flags, void* userData);

    void closeStdOut();
    void closeStdErr();

    void handleInput(int fd);
    void handleOutput(int fd, String& buffer, int& index, signalslot::Signal0& signal);

private:
    int mStdIn[2];
    int mStdOut[2];
    int mStdErr[2];

    pid_t mPid;
    int mReturn;

    std::deque<String> mStdInBuffer;
    String mStdOutBuffer, mStdErrBuffer;
    int mStdInIndex, mStdOutIndex, mStdErrIndex;

    Path mCwd;

    String mErrorString;

    signalslot::Signal0 mReadyReadStdOut, mReadyReadStdErr, mFinished;
};

#endif
