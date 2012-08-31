#ifndef MAKEFILEPARSER_H
#define MAKEFILEPARSER_H

#include "Path.h"
#include "GccArguments.h"
#include <List.h>
#include <Map.h>
#include <SignalSlot.h>

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
    Path makefile() const { return mMakefile; }
    Connection *connection() const { return mConnection; }
    signalslot::Signal1<MakefileParser*> &done() { return mDone; }
    signalslot::Signal2<const GccArguments &, MakefileParser*> &fileReady() { return mFileReady; }

    int sourceCount() const { return mSourceCount; }
    Map<Path, List<ByteArray> > &pendingFiles() { return mPendingFiles; }
    bool hasProject() const { return mHasProject; }
    void setHasProject(bool hasProject) { mHasProject = hasProject; }
private:
    void processMakeOutput();
    void processMakeError();
    void processMakeLine(const ByteArray &line);
    void onDone();

    Process *mProc;
    ByteArray mData;
    DirectoryTracker *mTracker;
    const List<ByteArray> mExtraFlags;
    int mSourceCount;
    Path mMakefile;
    Connection *mConnection;
    signalslot::Signal1<MakefileParser*> mDone;
    signalslot::Signal2<const GccArguments &, MakefileParser*> mFileReady;
    bool mHasProject;
    Map<Path, List<ByteArray> > mPendingFiles;
};

#endif // MAKEFILEPARSER_H
