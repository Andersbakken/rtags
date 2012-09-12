#include "MakefileParser.h"

#include "List.h"
#include "Log.h"
#include "Process.h"
#include "RTags.h"
#include "RegExp.h"
#include <stdio.h>

#ifndef MAKE
#define MAKE "make"
#endif

MakefileParser::MakefileParser(const List<ByteArray> &extraFlags, Connection *conn)
    : mProc(0), mExtraFlags(extraFlags), mSourceCount(0), mConnection(conn), mHasProject(false)
{
}

MakefileParser::~MakefileParser()
{
    if (mProc) {
        mProc->stop();
        delete mProc;
    }
}

void MakefileParser::run(const Path &makefile, const List<ByteArray> &args)
{
    Path make = MAKE;
    if (make.isAbsolute())
        make.resolve();
    mMakefile = makefile;
    assert(!mProc);
    mProc = new Process;

    mProc->readyReadStdOut().connect(this, &MakefileParser::processMakeOutput);
    mProc->readyReadStdErr().connect(this, &MakefileParser::processMakeError);
    mProc->finished().connect(this, &MakefileParser::onDone);

    mCurrentPath = makefile.parentDir();
    warning("%s -f %s -C %s\n",
            make.constData(),
            makefile.constData(),
            mCurrentPath.constData());

    List<ByteArray> a;
    a.push_back("-n");
    a.push_back("-f");
    a.push_back(makefile);
    a.push_back("-C");
    a.push_back(mCurrentPath);
    a.push_back("AM_DEFAULT_VERBOSITY=1");
    a.push_back("VERBOSE=1");

    const int c = args.size();
    for (int i=0; i<c; ++i) {
        a.push_back(args.at(i));
    }

    // unlink("/tmp/makelib.log");
    if (!mProc->start(make, a))
        error() << "Process failed" << mProc->errorString();
}

bool MakefileParser::isDone() const
{
    return mProc && mProc->isFinished();
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

void MakefileParser::processMakeError()
{
    assert(mProc);
    error("got stderr from make: '%s'", mProc->readAllStdErr().nullTerminated());
}

void MakefileParser::processMakeLine(const ByteArray &line)
{
    int from = -1;
    if (line.startsWith("RTAGS PWD=")) {
        const int pipe = line.indexOf('|', 10);
        if (!pipe) {
            error("Can't parse line, no pipe [%s]", line.constData());
            return;
        }
        const Path pwd = line.mid(10, pipe - 10);
        mCurrentPath = pwd;
        from = pipe + 1;
    } else if (line.startsWith("RTAGS|")) {
        from = 6;
    } else {
        return;
    }

    const ByteArray rest = line.mid(from);
    warning("Parsing line [%s] in [%s]\n", rest.constData(), mCurrentPath.constData());

    GccArguments args;
    if (args.parse(rest, mCurrentPath)) {
        args.addFlags(mExtraFlags);
        ++mSourceCount;
        fileReady()(args, this);
    }
}

void MakefileParser::onDone()
{
    done()(this);
}
