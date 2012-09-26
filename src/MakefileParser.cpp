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
    : mProc(0), mExtraCompilerFlags(extraFlags), mSourceCount(0), mConnection(conn)
{
}

MakefileParser::~MakefileParser()
{
    if (mProc) {
        mProc->stop();
        delete mProc;
    }
}

void MakefileParser::stop()
{
    delete mProc;
    mProc = 0; // ###???
}

void MakefileParser::run(const Path &makefile, const List<ByteArray> &arguments)
{
    // error() << makefile << arguments;
    List<ByteArray> args = arguments;
    bool noTricks = false;
    const int noTricksIndex = arguments.indexOf("<no-make-tricks>");
    if (noTricksIndex != -1) {
        args.removeAt(noTricksIndex);
        noTricks = true;
    }
    Path make;
    if (noTricks) {
#ifdef OS_FreeBSD
        make = "gmake";
#else
        make = "make";
#endif
    } else {
        make = MAKE;
    }
    mMakefile = makefile;
    assert(!mProc);
    mProc = new Process;

    mProc->readyReadStdOut().connect(this, &MakefileParser::processMakeOutput);
    mProc->readyReadStdErr().connect(this, &MakefileParser::processMakeError);
    mProc->finished().connect(this, &MakefileParser::onDone);

    mCurrentPath = makefile.parentDir();

    List<ByteArray> a;
    if (!noTricks)
        a.push_back("--dry-run");

    a.push_back("--makefile=" + makefile);
    a.push_back("--directory=" + mCurrentPath);
    a.push_back("AM_DEFAULT_VERBOSITY=1");
    a.push_back("VERBOSE=1");

    const int c = args.size();
    for (int i=0; i<c; ++i) {
        a.push_back(args.at(i));
    }

    warning("%s %s", make.constData(), ByteArray::join(a, " ").constData());

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
        // error("Parsed line [%s] in [%s] => [%s]", rest.constData(), mCurrentPath.constData(), args.inputFiles().value(0).constData());

        args.addFlags(mExtraCompilerFlags);
        ++mSourceCount;
        fileReady()(args, this);
    // } else {
    //     error("This didn't mean anything to me: [%s] in %s", rest.constData(), mCurrentPath.constData());
    }
}

void MakefileParser::onDone()
{
    done()(this);
}
