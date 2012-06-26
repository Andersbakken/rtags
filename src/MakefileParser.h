#ifndef MAKEFILEPARSER_H
#define MAKEFILEPARSER_H

#include "Path.h"
#include "GccArguments.h"
#include <List.h>
#include <Map.h>
#include <signalslot.h>

class DirectoryTracker;
class Connection;
class Process;

class MakefileParser
{
public:
    MakefileParser(const List<ByteArray> &extraFlags, Connection *conn);
    ~MakefileParser();

    void run(const Path &makefile, const List<ByteArray> &args);
    bool isDone() const;
    List<ByteArray> extraFlags() const { return mExtraFlags; }
    List<ByteArray> mapPchToInput(const List<ByteArray> &input) const;
    void setPch(const ByteArray &output, const ByteArray &input);
    Path makefile() const { return mMakefile; }
    Connection *connection() const { return mConnection; }
    signalslot::Signal1<MakefileParser*> &done() { return mDone; }
    signalslot::Signal1<const GccArguments &> &fileReady() { return mFileReady; }

    int sourceCount() const { return mSourceCount; }
    int pchCount() const { return mPchCount; }
private:
    void processMakeOutput();
    void processMakeLine(const ByteArray &line);
    void onDone();

    Process *mProc;
    ByteArray mData;
    DirectoryTracker *mTracker;
    const List<ByteArray> mExtraFlags;
    Map<ByteArray, ByteArray> mPchs;
    int mSourceCount, mPchCount;
    Path mMakefile;
    Connection *mConnection;
    signalslot::Signal1<MakefileParser*> mDone;
    signalslot::Signal1<const GccArguments &> mFileReady;
};

#endif // MAKEFILEPARSER_H
