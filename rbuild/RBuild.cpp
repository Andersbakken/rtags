#include "RBuild.h"
#include "GccArguments.h"
#include <QCoreApplication>
#include "Utils.h"
#include "Node.h"
#include "ClangRunnable.h"
#include <magic.h>
#include <fcntl.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/mman.h>
#include "PreprocessorRunnable.h"
#include <string.h>

RBuild::RBuild(int threadPoolCount, const QList<Path> &stdIncludePaths, QObject *parent)
    : QObject(parent), mDatabaseMode(Build), mPreprocessing(0), mParsing(0)
{
    foreach(const Path &p, stdIncludePaths)
        mPCHCompilerSwitches.insert("-I" + p);
    mThreadPool.setMaxThreadCount(threadPoolCount);
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
}

bool RBuild::addMakefile(Path makefile)
{
    if (!makefile.isResolved())
        makefile.resolve();

    // if (makefile.isFile()) {
    // qDebug() << makefile << findIncludes(makefile, mIndex, mIncludeFiles);
    // return true;
    // }
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
        MakefileData data = { makefile, workingDir, QByteArray(), pathStack, QSet<Path>() };
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
    // if (sourceDir.isDir()) {
    //     recurseDir(sourceDir);
    // }
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
    text += "|";
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
    maybePCH();
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

static inline bool isSupportedLanguage(GccArguments::Language l)
{
    switch (l) {
    case GccArguments::LangCPlusPlus:
    case GccArguments::LangC:
        return true;
    case GccArguments::LangUndefined:
    case GccArguments::LangObjC:
    case GccArguments::LangObjCPlusPlus:
        break;
    }
    return false;

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
            // qDebug("%s", line.constData());
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
                    } else if (args.hasInput() && args.isCompile() && ::isSupportedLanguage(args.language())) {
                        foreach(const Path &file, args.input()) { // already resolved
                            if (!data.seen.contains(file)) {
                                data.seen.insert(file);
                                load(file, args);
                            }
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
    if (isFinished()) {
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
    --mParsing;
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
        qDebug("%p %p", buffer.buffer().constData(), data->memory);
    }
    Q_ASSERT(buffer.buffer().constData() == data->memory); // this is not ever copied right?
    QDataStream ds(&buffer);
    buffer.seek(data->fileDataPosition);

    ds >> ClangRunnable::sFiles;
    qDebug() << "read files" << ClangRunnable::sFiles.size() << "at" << data->fileDataPosition;
    QSet<Path> modifiedPaths;
    QMutexLocker lock(&ClangRunnable::sTreeMutex);
    bool doInitTree = false;
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
            doInitTree = true;
            modifiedPaths.insert(key);
            load(key, value.arguments);
        } else {
            qDebug() << "not loading" << key;
        }
    }
    if (doInitTree)
        ClangRunnable::initTree(data, modifiedPaths);

    munmap(const_cast<char*>(data->memory), data->mappedSize);
    return true;
}

// static CXChildVisitResult dumpTree(CXCursor cursor, CXCursor, CXClientData includes)
// {
//     if (clang_getCursorKind(cursor) == CXCursor_InclusionDirective) {
//         QByteArray p = eatString(clang_getCursorSpelling(cursor));
//         reinterpret_cast<QSet<QByteArray>*>(includes)->insert(p);
//     }
//     return CXChildVisit_Continue;
// }

void RBuild::parseFile(const Path &file, const GccArguments &args, const char *pchFile)
{
    ClangRunnable *runnable = new ClangRunnable(file, args, pchFile);
    ++mParsing;
    connect(runnable, SIGNAL(finished()), this, SLOT(onClangRunnableFinished()));
    mThreadPool.start(runnable);
}

bool RBuild::isFinished() const
{
    return !mPreprocessing && !mParsing && mMakefiles.isEmpty();
}

void RBuild::preprocess(const Path &sourceFile, const GccArguments &args)
{
    ++mPreprocessing;
    // qDebug() << "calling preprocess" << sourceFile << args;
    PreprocessorRunnable *runnable = new PreprocessorRunnable(sourceFile, args);
    connect(runnable, SIGNAL(error(Path, GccArguments, QByteArray)),
            this, SLOT(onPreprocessorError(Path, GccArguments, QByteArray)));
    connect(runnable, SIGNAL(headersFound(Path, GccArguments, QList<Path>)),
            this, SLOT(onPreprocessorHeadersFound(Path, GccArguments, QList<Path>)));
    mThreadPool.start(runnable);
}

void RBuild::onPreprocessorError(const Path &sourceFile, const GccArguments &args, const QByteArray &error)
{
    --mPreprocessing;
    maybePCH();
    qWarning() << "onPreprocessorError" << sourceFile << args << error;
}
void RBuild::onPreprocessorHeadersFound(const Path &sourceFile, const GccArguments &args, const QList<Path> &headers)
{
    extern int verbose;
    int added = 0;
    foreach(const Path &header, headers) {
        if (header.contains("/private"))
            continue;
        QList<Path> &headers = (strcasestr(header.constData(), "x11")
                                ? mPostHeaders : mAllHeaders);
        if (!headers.contains(header)) {
            if (verbose > 2)
                qDebug("Adding %s for %s", header.constData(), sourceFile.constData());
            headers.append(header);
            ++added;
        }
    }
    mParsePending[sourceFile] = args;
    --mPreprocessing;
    // qDebug() << "onPreprocessorHeadersFound" << sourceFile << mPreprocessing;
    if (verbose)
        qDebug("Preprocessed %s, added %d headers", sourceFile.constData(), added);
    mPCHCompilerSwitches += args.arguments("-I").toSet();
    mPCHCompilerSwitches += args.arguments("-D").toSet();
    maybePCH();
    // qDebug() << sourceFile << mPCHCompilerSwitches << args.arguments();
    // qDebug() << "onPreprocessorHeadersFound" << sourceFile << args << headers << "Added" << added << "headers";
}

struct FindIncludersData {
    QSet<Path> errorLocations;
    QSet<Path> includers;
};

static inline void findIncluders(CXFile includedFile, CXSourceLocation* inclusionStack,
                                 unsigned includeIdx, CXClientData clientData)
{
    if (includeIdx <= 1) {
        return;
    }

    const Path file = eatString(clang_getFileName(includedFile));
    FindIncludersData *data = reinterpret_cast<FindIncludersData*>(clientData);
    if (data->errorLocations.contains(file)) {
        CXFile file;
        unsigned l, c, o;
        clang_getInstantiationLocation(inclusionStack[includeIdx - 2], &file, &l, &c, &o);
        data->includers.insert(eatString(clang_getFileName(file)));
        for (uint i=0; i<includeIdx; ++i) {
            CXFile file;
            unsigned l, c, o;
            clang_getInstantiationLocation(inclusionStack[i], &file, &l, &c, &o);
            qDebug() << i << eatString(clang_getFileName(file));
        }
        qDebug() << "found a file" << file << "adding"
                 << eatString(clang_getFileName(file));
    }
}

void RBuild::maybePCH()
{
    if (!mPreprocessing && mMakefiles.isEmpty()) {
        if (mParsePending.isEmpty()) {
            maybeDone();
            return;
        }
        if (!mAllHeaders.isEmpty() || !mPostHeaders.isEmpty()) {
            QElapsedTimer timer;
            timer.start();
            bool retry;
            do {
                retry = false;
                QByteArray pchHeader;
                pchHeader.reserve(mAllHeaders.size() * 48); // ###?
                pchHeader.append("#ifndef RTAGS_PCH_H\n#define RTAGS_PCH_H\n");
                foreach(const Path &header, mAllHeaders) {
                    pchHeader.append("#include \"" + header + "\"\n");
                }

                foreach(const Path &header, mPostHeaders) {
                    pchHeader.append("#include \"" + header + "\"\n");
                }

                pchHeader.append("#endif");
                // qDebug() << pchHeader;

                CXIndex index = clang_createIndex(1, 1);

                QVector<const char*> clangArgs(mPCHCompilerSwitches.size() + 2);
                int idx = 0;
                foreach(const QByteArray &s, mPCHCompilerSwitches) {
                    clangArgs[idx++] = s.constData();
                }

                clangArgs[idx++] = "-x";
                clangArgs[idx++] = "c++";
                char pchHeaderName[PATH_MAX] = { 0 };
                const char *tmpl = "/tmp/rtags.pch.h.XXXXXX";
                memcpy(pchHeaderName, tmpl, strlen(tmpl));
                const int pchHeaderFd = mkstemp(pchHeaderName);
                if (pchHeaderFd <= 0 || write(pchHeaderFd, pchHeader.constData(), pchHeader.size()) != pchHeader.size()) {
                    qWarning("PCH header write failure %s", pchHeaderName);
                    return;
                }
                // qWarning() << "building pch" << clangArgs;
                CXTranslationUnit unit = clang_parseTranslationUnit(index, pchHeaderName, clangArgs.constData(),
                                                                    idx, 0, 0,
                                                                    CXTranslationUnit_Incomplete);
                // qDebug() << mAllHeaders << clangArgs << pchHeader;
                if (!unit) {
                    qWarning() << clangArgs << pchHeader;
                    qFatal("Can't PCH this. That's no good");
                }
                extern int verbose;
                if (verbose)
                    qDebug("Created precompiled header (%d headers) %lldms", mAllHeaders.size(), timer.elapsed());
                // qDebug() << pchHeader;
                mPCHFile = "/tmp/rtags.pch.XXXXXX";
                if (mkstemp(mPCHFile.data()) <= 0) {
                    qWarning("PCH write failure %s", mPCHFile.constData());
                    clang_disposeTranslationUnit(unit);
                    return;
                }
                const int ret = clang_saveTranslationUnit(unit, mPCHFile.constData(), clang_defaultSaveOptions(unit));
                if (ret) {
                    FindIncludersData findIncludersData;
                    qWarning("Couldn't save translation unit %d", ret);
                    QByteArray out;
                    out.reserve(256);
                    out += QUOTE(CLANG_EXECUTABLE);
                    for (int i=0; i<idx; ++i) {
                        out += ' ';
                        out += clangArgs.at(i);
                    }
                    out += ' ';
                    out += pchHeaderName;
                    qDebug("%s", out.constData());
                    const int count = clang_getNumDiagnostics(unit);
                    for (int i=0; i<count; ++i) {
                        CXDiagnostic diagnostic = clang_getDiagnostic(unit, i);
                        const CXDiagnosticSeverity severity = clang_getDiagnosticSeverity(diagnostic);
                        if (severity >= CXDiagnostic_Error) {
                            QByteArray file = eatString(clang_formatDiagnostic(diagnostic, CXDiagnostic_DisplaySourceLocation
                                                                               |CXDiagnostic_DisplayColumn));
                            const int idx = file.indexOf(severity == CXDiagnostic_Error ? ": error: " : ": fatal: ");
                            if (idx != -1) {
                                Location loc(file.left(idx).constData());
                                qDebug() << loc << file.left(idx) << loc.path;
                                if (!mPostHeaders.removeOne(loc.path) && !mAllHeaders.removeOne(loc.path)) {
                                    findIncludersData.errorLocations.insert(loc.path);
                                } else {
                                    retry = true;
                                }
                            }
                        }
                        CXString diagStr = clang_getDiagnosticSpelling(diagnostic);
                        const unsigned diagnosticFormattingOptions = (CXDiagnostic_DisplaySourceLocation|CXDiagnostic_DisplayColumn|
                                                                      CXDiagnostic_DisplaySourceRanges|CXDiagnostic_DisplayOption|
                                                                      CXDiagnostic_DisplayCategoryId|CXDiagnostic_DisplayCategoryName);
                
                        CXString diagStr2 = clang_formatDiagnostic(diagnostic, diagnosticFormattingOptions);
                        qWarning() << "pch" << clang_getCString(diagStr) << clang_getCString(diagStr2) << clang_getDiagnosticSeverity(diagnostic);
                        clang_disposeString(diagStr);
                        clang_disposeString(diagStr2);
                        clang_disposeDiagnostic(diagnostic);
                    }
                    if (!findIncludersData.errorLocations.isEmpty()) {
                        clang_getInclusions(unit, findIncluders, &findIncludersData);
                        qDebug() << "we have some files to remove" << findIncludersData.includers;
                        foreach(const Path &includer, findIncludersData.includers) {
                            const bool found = (mPostHeaders.removeOne(includer) || mAllHeaders.removeOne(includer));
                            (void)found;
                            qDebug() << found;
                            Q_ASSERT(found);
                            retry = true;
                        }
                    }
                    mPCHFile.clear();
                } else {
                    ClangRunnable::processTranslationUnit(pchHeaderName, unit);
                }
                clang_disposeTranslationUnit(unit);
            } while (retry);
        }
        for (QHash<Path, GccArguments>::const_iterator it = mParsePending.begin(); it != mParsePending.end(); ++it) {
            parseFile(it.key(), it.value(), mPCHFile.constData());
        }
    }
}

    void RBuild::load(const Path &file, const GccArguments &args)
    {
        static const bool nopch = getenv("RTAGS_NO_PCH");
        if (!nopch && args.language() == GccArguments::LangCPlusPlus) {
            preprocess(file, args);
        } else {
            parseFile(file, args, 0);
        }
    }
