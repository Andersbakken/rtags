#include "MakefileParser.h"
#include <QDir>
#include <QCoreApplication>
#include <QStack>
#include <Log.h>
#include <stdio.h>

#ifndef MAKE
#define MAKE "make"
#endif

class DirectoryTracker
{
public:
    DirectoryTracker();

    void init(const Path& path);
    void track(const ByteArray& line);

    const Path& path() const { return mPaths.top(); }

private:
    void enterDirectory(const ByteArray& dir);
    void leaveDirectory(const ByteArray& dir);

private:
    QStack<Path> mPaths;
};

DirectoryTracker::DirectoryTracker()
{
}

void DirectoryTracker::init(const Path& path)
{
    mPaths.push(path);
}

void DirectoryTracker::track(const ByteArray& line)
{
    // printf("Tracking %s\n", line.constData());
    static QRegExp drx(QLatin1String("make[^:]*: ([^ ]+) directory `([^']+)'"));
    if (drx.indexIn(QString::fromLocal8Bit(line.constData())) != -1) {
        if (drx.cap(1) == QLatin1String("Entering")) {
            enterDirectory(drx.cap(2).toStdString());
        } else if (drx.cap(1) == QLatin1String("Leaving")) {
            leaveDirectory(drx.cap(2).toStdString());
        } else {
            qFatal("Invalid directory track: %s %s",
                   drx.cap(1).toLatin1().constData(),
                   drx.cap(2).toLatin1().constData());
        }
    }
}

void DirectoryTracker::enterDirectory(const ByteArray& dir)
{
    bool ok;
    Path newPath = Path::resolved(dir, path(), &ok);
    if (ok) {
        mPaths.push(newPath);
        debug("New directory resolved: %s", newPath.constData());
    } else {
        qFatal("Unable to resolve path %s (%s)", dir.constData(), path().constData());
    }
}

void DirectoryTracker::leaveDirectory(const ByteArray& dir)
{
    verboseDebug() << "leaveDirectory" << dir;
    // enter and leave share the same code for now
    mPaths.pop();
    // enterDirectory(dir);
}

MakefileParser::MakefileParser(const List<ByteArray> &extraFlags, QObject *parent)
    : QObject(parent), mProc(0), mTracker(new DirectoryTracker), mExtraFlags(extraFlags),
      mSourceCount(0), mPchCount(0)
{
}

MakefileParser::~MakefileParser()
{
    if (mProc) {
        mProc->kill();
        mProc->terminate();
        mProc->waitForFinished();
        delete mProc;
    }
    delete mTracker;
}

void MakefileParser::run(const Path &makefile, const List<ByteArray> &args)
{
    mMakefile = makefile;
    Q_ASSERT(!mProc);
    mProc = new QProcess(this);

    QDir makelibdir(QCoreApplication::applicationDirPath());
    makelibdir.cdUp();
    debug("Using makelib in '%s/makelib'", qPrintable(makelibdir.canonicalPath()));

    QProcessEnvironment environment = QProcessEnvironment::systemEnvironment();
    if (!args.contains("-B")) {
#ifdef Q_OS_MAC
        environment.insert("DYLD_INSERT_LIBRARIES", makelibdir.canonicalPath() + "/makelib/libmakelib.dylib");
#else
        environment.insert("LD_PRELOAD", makelibdir.canonicalPath() + "/makelib/libmakelib.so");
#endif
    }
    mProc->setProcessEnvironment(environment);

    connect(mProc, SIGNAL(readyReadStandardOutput()),
            this, SLOT(processMakeOutput()));
    connect(mProc, SIGNAL(readyReadStandardError()),
            this, SLOT(onReadyReadStandardError()));

    connect(mProc, SIGNAL(stateChanged(QProcess::ProcessState)),
            this, SLOT(onProcessStateChanged(QProcess::ProcessState)));

    connect(mProc, SIGNAL(error(QProcess::ProcessError)),
            this, SLOT(onError(QProcess::ProcessError)));
    connect(mProc, SIGNAL(finished(int)), this, SLOT(onDone()));

    mTracker->init(makefile.parentDir());
    warning(MAKE " -j1 -w -f %s -C %s\n",
            makefile.constData(), mTracker->path().constData());
    QStringList a;
    a << QLatin1String("-j1") << QLatin1String("-w")
      << QLatin1String("-f") << QString::fromLocal8Bit(makefile.constData(), makefile.size())
      << QLatin1String("-C") << QString::fromStdString(mTracker->path())
      << QLatin1String("AM_DEFAULT_VERBOSITY=1") << QLatin1String("VERBOSE=1");

    foreach(const ByteArray &arg, args) {
        a << QString::fromStdString(arg);
    }

    mProc->start(QLatin1String(MAKE), a);
}

bool MakefileParser::isDone() const
{
    return mProc && (mProc->state() == QProcess::NotRunning);
}

void MakefileParser::processMakeOutput()
{
    Q_ASSERT(mProc);
    mData += mProc->readAllStandardOutput().constData();

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
        emit fileReady(args);
    } else {
        mTracker->track(line);
    }
}
void MakefileParser::onError(QProcess::ProcessError err)
{
    error() << "Error" << int(err) << mProc->errorString().toStdString();
}
void MakefileParser::onProcessStateChanged(QProcess::ProcessState state)
{
    debug() << "process state changed" << state;
}
void MakefileParser::onReadyReadStandardError()
{
    debug() << "stderr" << mProc->readAllStandardError().constData();
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
    emit done(mSourceCount, mPchCount);
}
