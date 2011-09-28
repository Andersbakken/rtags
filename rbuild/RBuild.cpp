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
    : QObject(parent), mPendingRunnables(0), mDatabaseMode(Build), mPendingWork(false),
      mIndex(clang_createIndex(0, 0)), mPCHDirty(false)
{
    mThreadPool.setMaxThreadCount(qMax<int>(4, QThread::idealThreadCount() * 1.5));
}

RBuild::~RBuild()
{
    for (QHash<QProcess*, MakefileData>::const_iterator it = mMakefiles.begin(); it != mMakefiles.end(); ++it) {
        if (it.key()->state() != QProcess::NotRunning) {
            disconnect(it.key(), 0, this, 0); // don't want it to change mMakefiles upon exit
            it.key()->kill();
            it.key()->waitForFinished(2000); // ### ???
        }
    }
    clang_disposeIndex(mIndex);
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
            // printf("%s\n", line.constData());
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
        // QCoreApplication::quit();
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

static CXChildVisitResult dumpTree(CXCursor cursor, CXCursor, CXClientData includes)
{
    if (clang_getCursorKind(cursor) == CXCursor_InclusionDirective) {
        QByteArray p = eatString(clang_getCursorSpelling(cursor));
        reinterpret_cast<QSet<QByteArray>*>(includes)->insert(p);
    }
    return CXChildVisit_Continue;
}

static inline void gatherHeaders(CXFile includedFile, CXSourceLocation*,
                                 unsigned includeLen, CXClientData userData)
{
    const QByteArray filename = eatString(clang_getFileName(includedFile));
    qWarning() << filename << includeLen;

    if (!includeLen)
        return;

#warning fix
    if (!filename.contains("/bits/")) {
        QSet<Path> *includes = reinterpret_cast<QSet<Path>*>(userData);
        includes->insert(Path::resolved(filename));
    }
    // if (include_len == 1)
    //     data->direct.append(rfn);
    // data->all.append(rfn);
}

static Path buildPCH(const QSet<Path> &headers, const GccArguments &args, CXIndex idx)
{
    qDebug() << "buildPCH" << headers;
    QByteArray inc;
    // inc += "#include \"/home/abakken/dev/qt-47/include/QtCore/qhash.h\"\n";
    foreach(const Path &header, headers) {
        inc += "#include \"" + header + "\"\n";
    }


    int cnt = 0;
    Path fn;
    char buf[32];
    do {
        const int len = snprintf(buf, 32, "/tmp/rtags_header%d.pch", cnt);
        fn = QByteArray(buf, len);
        ++cnt;
    } while (fn.exists());
    fn += ".h";

    QFile inp(fn);
    if (!inp.open(QFile::WriteOnly)) {
        return Path();
    }
    inp.write(inc);
    inp.close();
    const QList<QByteArray> arguments = args.arguments("-D") + args.includePaths();
    QVarLengthArray<const char*, 32> pchArgs(arguments.size() + 2);
    int i = 0;
    foreach(const QByteArray &arg, arguments) {
        pchArgs[i++] = arg.constData();
    }
    pchArgs[i++] = "-x";
    pchArgs[i++] = "c++";

    for (int i=0; i<pchArgs.size(); ++i) {
        qDebug() << pchArgs[i];
    }
    
    CXTranslationUnit unit = clang_parseTranslationUnit(idx, fn.constData(), pchArgs.constData(), pchArgs.size(),
                                                        0, 0, CXTranslationUnit_Incomplete);
    if (!unit) {
        for (int i=0; i<pchArgs.size(); ++i) {
            qDebug() << pchArgs[i];
        }

        // qDebug() << fn << pchArgs << pchArgs.size();
        // printf("%s %d: if (!unit) {\n", __FILE__, __LINE__);
        return Path();
    }
   
    for (unsigned int i = 0; i < clang_getNumDiagnostics(unit); ++i) {
        CXDiagnostic diag = clang_getDiagnostic(unit, i);
        CXString diagstr = clang_getDiagnosticSpelling(diag);
        qDebug() << clang_getCString(diagstr);
        clang_disposeString(diagstr);
        clang_disposeDiagnostic(diag);
    }

    Path ret(buf);
    const int res = clang_saveTranslationUnit(unit, buf, 0);
    qDebug() << "saved pch" << res << buf;
    ClangRunnable::processTranslationUnit(ret, unit);
    clang_disposeTranslationUnit(unit);
    return ret;
}

void RBuild::startRunnable(const Path &file, const GccArguments &args)
{
    ClangRunnable *runnable = new ClangRunnable(file, args, mLastPCH);
    ++mPendingRunnables;
    connect(runnable, SIGNAL(finished()), this, SLOT(onClangRunnableFinished()));
    mThreadPool.start(runnable);
}

bool RBuild::addFile(const Path &file, const GccArguments &args)
{
    if (file.exists()) {
        CXTranslationUnit unit = clang_parseTranslationUnit(mIndex, file.constData(),
                                                            0, 0, 0, 0, 0);
        if (!unit) {
            qWarning("Couldn't parse %s", file.constData());
            return false;
        }
        const QSet<Path> old = mIncludeFiles;
        clang_getInclusions(unit, gatherHeaders, &mIncludeFiles);
        clang_disposeTranslationUnit(unit);
        bool doPCH = false;
        if (old.size() == mIncludeFiles.size()) {
            if (!mPCHDirty) {
                Q_ASSERT(mPendingFiles.isEmpty());
                startRunnable(file, args);
                return true;
            } else {
                if (mPendingFiles.size() + 1 >= 10) {
                    doPCH = true;
                } else {
                    mPendingFiles[file] = args;
                }
            }
        } else if (++mPCHDirty >= 3) {
            doPCH = true;
        }

        if (doPCH) {
            mLastPCH = buildPCH(mIncludeFiles, args, mIndex);
            mPCHDirty = 0;
            startRunnable(file, args);
            for (QHash<Path, GccArguments>::const_iterator it = mPendingFiles.begin(); it != mPendingFiles.end(); ++it) {
                startRunnable(it.key(), it.value());
            }
            mPendingFiles.clear();
        } else {
            qDebug() << mPendingFiles << mIncludeFiles << mPCHDirty;
        }
    }
    return false;
}

bool RBuild::pendingWork() const
{
    return mPendingWork;
}
