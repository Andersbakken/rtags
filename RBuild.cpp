#include "RBuild.h"
#include "GccArguments.h"
#include <QCoreApplication>
#include "Utils.h"
#include "PreCompile.h"
#include "Node.h"
#include "ClangRunnable.h"
#include <magic.h>
#include <fcntl.h>

RBuild::RBuild(QObject *parent)
    : QObject(parent), mPendingRunnables(0)
{
    qDebug() << QThread::idealThreadCount();
    mThreadPool.setMaxThreadCount(QThread::idealThreadCount());
}

void RBuild::addMakefile(Path makefile)
{
    if (!makefile.isResolved())
        makefile.resolve();
    if (makefile.isDir())
        makefile = makefile + "/Makefile";
    if (makefile.isFile()) {
        const Path workingDir = makefile.parentDir();
        if (!makefile.exists())
            return;
        QDir::setCurrent(workingDir); // ### hmmmm
        QProcess *proc = new QProcess(this);
        proc->setWorkingDirectory(workingDir);
        // proc->moveToThread(this);
        connect(proc, SIGNAL(finished(int)), this, SLOT(onMakeFinished(int)));
        connect(proc, SIGNAL(error(QProcess::ProcessError)), this, SLOT(onMakeError(QProcess::ProcessError)));
        connect(proc, SIGNAL(readyReadStandardOutput()), this, SLOT(onMakeOutput()));
        MakefileData data = { makefile, workingDir, QByteArray(), QSet<Path>(), workingDir };
        mMakefiles[proc] = data;
        proc->start(QLatin1String("make"), // some way to specify which make to use?
                    QStringList()
                    << QLatin1String("-B")
                    << QLatin1String("-n")
                    << QLatin1String("-j1")
                    << QLatin1String("-p")
                    << QLatin1String("-f")
                    << makefile);
        qDebug() << "addMakefile" << makefile;
    }
}

QDebug operator<<(QDebug dbg, CXCursor cursor)
{
    QString text = "";
    if (clang_isInvalid(clang_getCursorKind(cursor))) {
        text += "";
        dbg << text;
        return dbg;
    }

    QByteArray name = eatString(clang_getCursorDisplayName(cursor));
    if (name.isEmpty())
        name = eatString(clang_getCursorSpelling(cursor));
    if (!name.isEmpty()) {
        text += name + ", ";
    }
    if (clang_isCursorDefinition(cursor))
        text += "(def), ";
    text += kindToString(clang_getCursorKind(cursor));
    CXSourceLocation location = clang_getCursorLocation(cursor);
    unsigned int line, column, offset;
    CXFile file;
    clang_getInstantiationLocation(location, &file, &line, &column, &offset);
    Path path = eatString(clang_getFileName(file));
    if (path.resolve()) {
        text += QString(", %1:%2:%3").arg(QString::fromLocal8Bit(path)).arg(line).arg(column);
    }
    if (clang_isCursorDefinition(cursor))
        text += ", def";
    dbg << text;
    return dbg;
}

void RBuild::onMakeFinished(int statusCode)
{
    QProcess *proc = qobject_cast<QProcess*>(sender());
    if (statusCode != 0) {
        qWarning("make error: %s", proc->readAllStandardError().constData());
    }
    mMakefiles.remove(proc);
    proc->deleteLater();
    maybeDone();
}

void RBuild::onMakeOutput()
{
    QProcess *proc = qobject_cast<QProcess*>(sender());
    MakefileData &data = mMakefiles[proc];
    int i = data.buffer.size();
    data.buffer += proc->readAllStandardOutput();
    int last = 0;
    const int size = data.buffer.size();
    while (i < size) {
        if (data.buffer.at(i++) == '\n') {
            const QByteArray line(data.buffer.constData() + last, i - last - 1);
            last = i;
            if (!line.isEmpty()) {
                if (line.startsWith("cd ")) {
                    const int slash = line.indexOf('/', 3);
                    if (slash == -1) {
                        qWarning("Can't parse this line. Seems like a cd but no slash [%s]",
                                 line.constData());
                        continue;
                    }
                    Path p = line.mid(3, slash - 3);
                    if (!p.resolve(data.directory)) {
                        qWarning("Can't resolve directory %s with %s",
                                 line.mid(3, slash - 3).constData(),
                                 data.directory.constData());
                    } else {
                        data.workingDirectory = p;
                    }
                    continue;
                }

                GccArguments args;
                if (!args.parse(line, data.workingDirectory)) {
                    qWarning("Can't parse line %s (%s)", line.constData(),
                             qPrintable(args.errorString()));
                    continue;
                }
                if (!args.hasInput() || !args.isCompile()) {
                    QStringList strings = QString::fromLocal8Bit(line).split(' ');
                    const int size = strings.size();
                    if (size >= 3 && strings.first().endsWith(":")) {
                        const Path sourceFile = Path::resolved(strings.at(1).toLocal8Bit(), data.directory);
                        if (sourceFile.isSource()) { // using extension to determine if this is a source file
                            FileData &fd = mFiles[sourceFile];
                            for (int i=2; i<size; ++i) {
                                const Path header = Path::resolved(strings.at(i).toLocal8Bit(), data.directory);
                                if (header.isSource()) {
                                    mFiles[header].dependents.insert(sourceFile);
                                    fd.dependsOn.insert(header);
                                }
                            }
                        }
                    }
                } else {
                    foreach(const Path &file, args.input()) { // already resolved
                        Q_ASSERT(file.exists());
                        if (!data.seen.contains(file)) {
                            data.seen.insert(file);
                            // qDebug() << "setting arguments for" << file << "to" << args.raw();
                            ClangRunnable *runnable = new ClangRunnable(file, args);
                            ++mPendingRunnables;
                            connect(runnable, SIGNAL(finished()), this, SLOT(onClangRunnableFinished()));
                            mThreadPool.start(runnable);
                        }
                    }
                }
            }
        }
    }
    if (i < size) {
        data.buffer.remove(0, last);
    } else {
        data.buffer.clear();
    }
}

void RBuild::onMakeError(QProcess::ProcessError error)
{
    qWarning() << error << qobject_cast<QProcess*>(sender())->errorString();
}

void RBuild::maybeDone()
{
    if (!mPendingRunnables) {
        for (QHash<QProcess*, MakefileData>::const_iterator it = mMakefiles.begin(); it != mMakefiles.end(); ++it) {
            if (it.key() && it.key()->state() != QProcess::NotRunning) {
                return;
            }
        }
        QCoreApplication::quit();
    }
}
void RBuild::onClangRunnableFinished()
{
    --mPendingRunnables;
    maybeDone();
}
