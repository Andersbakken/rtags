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
    void track(const QByteArray& line);

    const Path& path() const { return mPaths.top(); }

private:
    void enterDirectory(const QByteArray& dir);
    void leaveDirectory(const QByteArray& dir);

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

void DirectoryTracker::track(const QByteArray& line)
{
    // printf("Tracking %s\n", line.constData());
    static QRegExp drx(QLatin1String("make[^:]*: ([^ ]+) directory `([^']+)'"));
    if (drx.indexIn(QString::fromLocal8Bit(line.constData())) != -1) {
        if (drx.cap(1) == QLatin1String("Entering"))
            enterDirectory(drx.cap(2).toLocal8Bit());
        else if (drx.cap(1) == QLatin1String("Leaving"))
            leaveDirectory(drx.cap(2).toLocal8Bit());
        else {
            qFatal("Invalid directory track: %s %s",
                   drx.cap(1).toLatin1().constData(),
                   drx.cap(2).toLatin1().constData());
        }
    }
}

void DirectoryTracker::enterDirectory(const QByteArray& dir)
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

void DirectoryTracker::leaveDirectory(const QByteArray& dir)
{
    verboseDebug() << "leaveDirectory" << dir;
    // enter and leave share the same code for now
    mPaths.pop();
    // enterDirectory(dir);
}

MakefileParser::MakefileParser(const QList<QByteArray> &extraFlags, QObject* parent)
    : QObject(parent), mProc(0), mTracker(new DirectoryTracker), mExtraFlags(extraFlags)
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

void MakefileParser::run(const Path& makefile)
{
    Q_ASSERT(!mProc);
    mProc = new QProcess(this);

    QDir makelibdir(QCoreApplication::applicationDirPath());
    makelibdir.cdUp();
    if (logLevel() > 0)
        debug("Using makelib in '%s/makelib'", qPrintable(makelibdir.canonicalPath()));

    QProcessEnvironment environment = QProcessEnvironment::systemEnvironment();
#ifdef Q_OS_MAC
    environment.insert("DYLD_INSERT_LIBRARIES", makelibdir.canonicalPath() + "/makelib/libmakelib.dylib");
#else
    environment.insert("LD_PRELOAD", makelibdir.canonicalPath() + "/makelib/libmakelib.so");
#endif
    mProc->setProcessEnvironment(environment);

    connect(mProc, SIGNAL(readyReadStandardOutput()),
            this, SLOT(processMakeOutput()));
    connect(mProc, SIGNAL(readyReadStandardError()),
            this, SLOT(onReadyReadStandardError()));

    connect(mProc, SIGNAL(stateChanged(QProcess::ProcessState)),
            this, SLOT(onProcessStateChanged(QProcess::ProcessState)));

    connect(mProc, SIGNAL(error(QProcess::ProcessError)),
            this, SLOT(onError(QProcess::ProcessError)));
    connect(mProc, SIGNAL(finished(int)), this, SIGNAL(done()));

    mTracker->init(makefile.parentDir());
    if (logLevel()) {
        log(1, MAKE " -j1 -n -w -f %s -C %s\n",
            makefile.constData(), mTracker->path().constData());
    }
    mProc->start(QLatin1String(MAKE), QStringList()
                 << QLatin1String("-j1") << QLatin1String("-n") << QLatin1String("-w")
                 << QLatin1String("-f") << QString::fromLocal8Bit(makefile)
                 << QLatin1String("-C") << mTracker->path());
}

bool MakefileParser::isDone() const
{
    return mProc && (mProc->state() == QProcess::NotRunning);
}

void MakefileParser::processMakeOutput()
{
    Q_ASSERT(mProc);
    mData += mProc->readAllStandardOutput();

    int nextNewline = mData.indexOf('\n');
    while (nextNewline != -1) {
        processMakeLine(mData.left(nextNewline));
        mData = mData.mid(nextNewline + 1);
        nextNewline = mData.indexOf('\n');
    }
}

void MakefileParser::processMakeLine(const QByteArray &line)
{
    if (testLog(VerboseDebug))
        verboseDebug("%s", line.constData());
    GccArguments args;
    if (args.parse(line, mTracker->path())) {
        args.addFlags(mExtraFlags);
        emit fileReady(args);
    } else {
        mTracker->track(line);
    }
}
void MakefileParser::onError(QProcess::ProcessError err)
{
    error() << "Error" << err << mProc->errorString();
}
void MakefileParser::onProcessStateChanged(QProcess::ProcessState state)
{
    debug() << "process state changed" << state;
}
void MakefileParser::onReadyReadStandardError()
{
    debug() << "stderr" << mProc->readAllStandardError();
}
