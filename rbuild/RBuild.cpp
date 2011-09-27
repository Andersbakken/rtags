#include "RBuild.h"
#include "GccArguments.h"
#include <QCoreApplication>
#include "Utils.h"
#include "PreCompile.h"
#include "Node.h"
#include "ClangRunnable.h"
#include <magic.h>
#include <fcntl.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/mman.h>

RBuild::RBuild(QObject *parent)
    : QObject(parent), mPendingRunnables(0), mDatabaseMode(Build), mPendingWork(false)
{
    mThreadPool.setMaxThreadCount(qMax(4, QThread::idealThreadCount() * 2));
    qDebug() << mThreadPool.maxThreadCount();
}

bool RBuild::addMakefile(Path makefile)
{
    if (!makefile.isResolved())
        makefile.resolve();
    Path sourceDir;
    if (makefile.isDir()) {
        sourceDir = makefile;
        makefile = makefile + "/Makefile";
    }
    if (makefile.isFile()) {
        if (sourceDir.isEmpty())
            sourceDir = makefile.parentDir();
        const Path workingDir = makefile.parentDir();
        if (!makefile.exists())
            return false;
        QDir::setCurrent(workingDir); // ### hmmmm
        QProcess *proc = new QProcess(this);
        proc->setWorkingDirectory(workingDir);
        // proc->moveToThread(this);
        connect(proc, SIGNAL(finished(int)), this, SLOT(onMakeFinished(int)));
        connect(proc, SIGNAL(error(QProcess::ProcessError)), this, SLOT(onMakeError(QProcess::ProcessError)));
        connect(proc, SIGNAL(readyReadStandardOutput()), this, SLOT(onMakeOutput()));
        QStack<Path> pathStack;
        pathStack.push(workingDir);
        MakefileData data = { makefile, workingDir, QByteArray(), pathStack };
        mMakefiles[proc] = data;
        QString make;
        if (Path("/opt/local/bin/gmake").isFile()) {
            make = QLatin1String("/opt/local/bin/gmake");
        } else {
            make = QLatin1String("make");
        }
        proc->start(make, // some way to specify which make to use?
                    QStringList()
                    << QLatin1String("-B")
                    << QLatin1String("-n")
                    << QLatin1String("-j1")
                    << QLatin1String("-w")
                    << QLatin1String("-f")
                    << makefile);
        qDebug() << "addMakefile" << makefile;
    } else {
        return false;
    }
    if (sourceDir.isDir()) {
        recurseDir(sourceDir);
    }
    mPendingWork = true;
    return true;
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

enum DirectoryStatus {
    None,
    Entering,
    Leaving
};
static inline DirectoryStatus parseDirectoryLine(const QByteArray &ba, Path &dir)
{
    if (ba.endsWith('\'')) {
        int pos = ba.indexOf("Entering directory `");
        if (pos != -1) {
            dir = ba.mid(pos + 20, ba.size() - pos - 21);
            return Entering;
        }
        pos = ba.indexOf("Leaving directory `");
        if (pos != -1) {
            dir = ba.mid(pos + 19, ba.size() - pos - 20);
            return Leaving;
        }
    }
    return None;
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
            printf("%s\n", line.constData());
            last = i;
            if (!line.isEmpty()) {
                Path dir;
                switch (parseDirectoryLine(line, dir)) {
                case Entering:
                    data.dirStack.push(dir);
                    break;

                case Leaving:
#ifdef QT_DEBUG
                    if (data.dirStack.top() != dir) {
                        qWarning() << "Strange make output"
                                   << data.dirStack << dir;
                    }
#endif
                    data.dirStack.pop();
                    break;
                case None: {
                    GccArguments args;
                    if (!args.parse(line, data.dirStack.top())) {
                        qWarning("Can't parse line %s (%s)", line.constData(),
                                 qPrintable(args.errorString()));
                    } else if (args.hasInput() && args.isCompile()) {
                        foreach(const Path &file, args.input()) { // already resolved
                            addFile(file, args);
                        }
                    }
                    break; }
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
        ClangRunnable::save(".rtags.db");
        QCoreApplication::quit();
    }
}
void RBuild::onClangRunnableFinished()
{
    --mPendingRunnables;
    maybeDone();
}

void RBuild::recurseDir(const Path &path)
{
    DIR *dir = opendir(path.constData());
    if (!dir) {
        qWarning("Can't read directory [%s]", path.constData());
        return;
    }
    struct dirent d, *dret;
    struct stat s;

    char fileBuffer[PATH_MAX + 1];
    memcpy(fileBuffer, path.constData(), path.size());
    fileBuffer[path.size()] = '/';
    char *file = fileBuffer + path.size() + 1;

    while (readdir_r(dir, &d, &dret) == 0 && dret) {
        Q_ASSERT(int(strlen(d.d_name)) < 1024 - path.size());
        if (d.d_type == DT_DIR && strcmp(".", d.d_name) && strcmp("..", d.d_name)) { // follow symlinks to dirs?
            recurseDir(path + '/' + reinterpret_cast<const char *>(d.d_name));
        } else if (d.d_type == DT_REG || d.d_type == DT_LNK) {
            strcpy(file, d.d_name);
            if (!stat(fileBuffer, &s)) {
                if (s.st_mode & S_IXOTH) {
                    // const Model::Item item = { fileBuffer, findIconPath(fileBuffer), name(fileBuffer), QStringList() };
                    // mLocalItems.append(item);
                }
            } else {
                qWarning("Can't stat [%s]", fileBuffer);
            }
        }
    }
    
    closedir(dir);
}
bool RBuild::setDatabaseFile(const Path &path, DatabaseMode mode)
{
    mDatabaseFile = path;
    mDatabaseMode = mode;
    if (mode == Update) {
        MMapData data;
        if (!loadDb(path.constData(), &data))
            return false;

        const bool success = initFromDb(&data);
        munmap(const_cast<char*>(data.memory), data.mappedSize);
        return success;
    }

    return true;
}

Path RBuild::databaseFile() const
{
    return mDatabaseFile;
}

bool RBuild::findDatabaseFile(DatabaseMode mode)
{
    char buf[PATH_MAX + 1];
    if (findDB(buf, PATH_MAX)) {
        return setDatabaseFile(buf, mode);
    } else if (mode == Build) {
        return setDatabaseFile(".rtags.db", mode);
    } else {
        return false;
    }
}
bool RBuild::initFromDb(const MMapData *data)
{
    const QByteArray mapped = QByteArray::fromRawData(reinterpret_cast<const char*>(data->memory), data->mappedSize);
    QBuffer buffer;
    buffer.setData(mapped);
    buffer.open(QIODevice::ReadOnly);
    if (buffer.buffer().constData() != data->memory) {
        printf("%p %p\n", buffer.buffer().constData(), data->memory);
    }
    Q_ASSERT(buffer.buffer().constData() == data->memory); // this is not ever copied right?
    QDataStream ds(&buffer);
    buffer.seek(data->fileDataPosition);

    ds >> ClangRunnable::sFiles;
    qDebug() << "read files" << ClangRunnable::sFiles.size() << "at" << data->fileDataPosition;
    QSet<Path> modifiedPaths;
    QMutexLocker lock(&ClangRunnable::sTreeMutex);
    for (QHash<Path, ClangRunnable::FileData>::const_iterator it = ClangRunnable::sFiles.begin();
         it != ClangRunnable::sFiles.end(); ++it) {
        const Path &key = it.key();
        const ClangRunnable::FileData &value = it.value();
        bool modified = (key.lastModified() != value.lastModified);
        // qDebug() << "checking" << key << key.lastModified() << value.lastModified;
        // ### this code needs to know about headers included with different #defines
        for (QHash<Path, int64_t>::const_iterator it = value.dependencies.begin();
             it != value.dependencies.end(); ++it) {
            if (modifiedPaths.contains(it.key())) {
                modified = true;
            } else if (it.key().lastModified() != it.value()) {
                modified = true;
                modifiedPaths.insert(it.key());
            }
        }
        if (modified) {
            modifiedPaths.insert(key);
            if (addFile(key, value.arguments)) {
                mPendingWork = true;
            }
        }
    }
    if (mPendingWork) {
        ClangRunnable::initTree(data, modifiedPaths);
    }

    munmap(const_cast<char*>(data->memory), data->mappedSize);
    return true;
}

bool RBuild::addFile(const Path &file, const GccArguments &args)
{
    if (file.exists()) {
        QList<GccArguments> &fileArgs = mSeen[file];
        if (!fileArgs.contains(args)) {
            fileArgs.append(args);
            // qDebug() << "setting arguments for" << file << "to" << args.raw();
            ClangRunnable *runnable = new ClangRunnable(file, args);
            ++mPendingRunnables;
            connect(runnable, SIGNAL(finished()), this, SLOT(onClangRunnableFinished()));
            mThreadPool.start(runnable);
            return true;
        }
    }
    return false;
}

bool RBuild::pendingWork() const
{
    return mPendingWork;
}
