#include "MakefileParser.h"
#include <Log.h>
#include <List.h>
#include <stdio.h>
#include <RegExp.h>
#include <RTags.h>
#include "Rdm.h"
#include "Process.h"

#ifndef MAKE
#define MAKE "make"
#endif

class DirectoryTracker
{
public:
    DirectoryTracker();

    void init(const Path &path);
    void track(const ByteArray &line);

    const Path &path() const { return mPaths.back(); }

private:
    void enterDirectory(const ByteArray &dir);
    void leaveDirectory(const ByteArray &dir);

private:
    List<Path> mPaths;
};

DirectoryTracker::DirectoryTracker()
{
}

void DirectoryTracker::init(const Path &path)
{
    mPaths.push_back(path);
}

void DirectoryTracker::track(const ByteArray &line)
{
    // printf("Tracking %s\n", line.constData());
    static RegExp rx("make[^:]*: ([^ ]+) directory `([^']+)'");
    List<RegExp::Capture> captures;
    if (rx.indexIn(line.constData(), 0, &captures) != -1) {
        assert(captures.size() >= 3);
        if (captures.at(1).capture() == "Entering") {
            enterDirectory(captures.at(2).capture());
        } else if (captures.at(1).capture() == "Leaving") {
            leaveDirectory(captures.at(2).capture());
        } else {
            error("Invalid directory track: %s %s",
                  captures.at(1).capture().constData(),
                  captures.at(2).capture().constData());
        }
    }
}

void DirectoryTracker::enterDirectory(const ByteArray &dir)
{
    bool ok;
    Path newPath = Path::resolved(dir, path(), &ok);
    if (ok) {
        mPaths.push_back(newPath);
        debug("New directory resolved: %s", newPath.constData());
    } else {
        qFatal("Unable to resolve path %s (%s)", dir.constData(), path().constData());
    }
}

void DirectoryTracker::leaveDirectory(const ByteArray &dir)
{
    verboseDebug() << "leaveDirectory" << dir;
    // enter and leave share the same code for now
    mPaths.pop_back();
    // enterDirectory(dir);
}

MakefileParser::MakefileParser(const List<ByteArray> &extraFlags, Connection *conn)
    : QObject(), mProc(0), mTracker(new DirectoryTracker), mExtraFlags(extraFlags),
      mSourceCount(0), mPchCount(0), mConnection(conn)
{
}

MakefileParser::~MakefileParser()
{
    if (mProc) {
        mProc->stop();
        delete mProc;
    }
    delete mTracker;
}

void MakefileParser::run(const Path &makefile, const List<ByteArray> &args)
{
    mMakefile = makefile;
    Q_ASSERT(!mProc);
    mProc = new Process;

    std::list<ByteArray> environment = Process::environment();
    if (!args.contains("-B")) {
        Path p = RTags::applicationDirPath();
#ifdef OS_Mac
        p += "/../makelib/libmakelib.dylib";
        p.resolve();
        environment.push_back("DYLD_INSERT_LIBRARIES=" + p);
#else
        p += "/../makelib/libmakelib.so";
        p.resolve();
        environment.push_back("LD_PRELOAD=" + p);
#endif
    }

    mProc->readyReadStdOut().connect(this, &MakefileParser::processMakeOutput);
    mProc->finished().connect(this, &MakefileParser::onDone);

    mTracker->init(makefile.parentDir());
    warning(MAKE " -j1 -w -f %s -C %s\n",
            makefile.constData(), mTracker->path().constData());

    std::list<ByteArray> a;
    a.push_back("-j1");
    a.push_back("-w");
    a.push_back("-f");
    a.push_back(makefile);
    a.push_back("-C");
    a.push_back(mTracker->path());
    a.push_back("AM_DEFAULT_VERBOSITY=1");
    a.push_back("VERBOSE=1");

    foreach(const ByteArray &arg, args) {
        a.push_back(arg);
    }

    unlink("/tmp/makelib.log");
    mProc->start(MAKE, a, environment);
}

bool MakefileParser::isDone() const
{
    return mProc && mProc->isDone();
}

void MakefileParser::processMakeOutput()
{
    assert(mProc);
    mData += mProc->readAllStdOut();

    // ### this could be more efficient
    int nextNewline = mData.indexOf('\n');
    while (nextNewline != -1) {
        processMakeLine(mData.left(nextNewline));
        mData = mData.mid(nextNewline + 1);
        nextNewline = mData.indexOf('\n');
    }
}

void MakefileParser::processMakeLine(const ByteArray &line)
{
    printf("processMakeLine '%s'\n", line.nullTerminated());
    if (testLog(VerboseDebug))
        verboseDebug("%s", line.constData());
    GccArguments args;
    if (args.parse(line, mTracker->path())) {
        args.addFlags(mExtraFlags);
        if (args.type() == GccArguments::Pch) {
            ++mPchCount;
        } else {
            ++mSourceCount;
        }
        fileReady()(args);
    } else {
        mTracker->track(line);
    }
}

List<ByteArray> MakefileParser::mapPchToInput(const List<ByteArray> &input) const
{
    List<ByteArray> output;
    Map<ByteArray, ByteArray>::const_iterator pchit;
    const Map<ByteArray, ByteArray>::const_iterator pchend = mPchs.end();
    foreach (const ByteArray &in, input) {
        pchit = mPchs.find(in);
        if (pchit != pchend)
            output.append(pchit->second);
    }
    return output;
}

void MakefileParser::setPch(const ByteArray &output, const ByteArray &input)
{
    mPchs[output] = input;
}

void MakefileParser::onDone()
{
    done()(this);
}
