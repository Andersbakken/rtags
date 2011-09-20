#include "ParseThread.h"
#include "PreCompile.h"
#include "FileManager.h"
#include "TemporaryFiles.h"
#include "VisitThread.h"

ParseThread::ParseThread(FileManager *fm, VisitThread *vt)
    : mAborted(false), mFirst(0), mLast(0), mCount(0),
      mIndex(clang_createIndex(1, 0)), mFileManager(fm),
      mVisitThread(vt)
{
    setObjectName("ParseThread");
    PreCompile::setPath("/tmp");
}

ParseThread::~ParseThread()
{
    while (mFirst) {
        File *f = mFirst->next;
        delete mFirst;
        mFirst = f;
    }
    clang_disposeIndex(mIndex);
}

void ParseThread::abort()
{
    mAborted = true;
    mWaitCondition.wakeOne();
}


// static inline bool isSource(const Path &path) // ### could check if we have GccArguments
// {
//     const int dot = path.lastIndexOf('.');
//     const int len = path.size() - dot - 1;
//     if (dot != -1 && len > 0) {
//         const char *sourceExtensions[] = { "c", "cpp", "cxx", "cc", 0 };
//         for (int i=0; sourceExtensions[i]; ++i) {
//             if (!strncasecmp(sourceExtensions[i], path.constData() + dot + 1, len)) {
//                 return true;
//             }
//         }
//     }
//     return false;
// }

// void Daemon::addDeps(const Path &path, QHash<Path, GccArguments> &deps, QSet<Path> &seen)
// {
//     GccArguments hack;
//     if (path.isFile() && !seen.contains(path)) {
//         // qDebug() << path << (path.lastModified() != mFiles.value(path));
//         seen.insert(path);
//         time_t &lastModified = mFiles[path];
//         const time_t current = path.lastModified();
//         GccArguments &args = lastModified == current ? hack : deps[path];
//         lastModified = current;
//         QSet<Path> dependsOn;
//         if (mFileManager.getInfo(path, &args, 0, &dependsOn)) {
//             foreach(const Path &dep, dependsOn) {
//                 QSet<Path> dependents;
//                 mFileManager.getInfo(dep, 0, &dependents, 0);
//                 foreach(const Path &dependent, dependents) {
//                     addDeps(dependent, deps, seen);
//                 }
//                 addDeps(dep, deps, seen);
//             }
//         }
//     }
// }

    // int added = 0;
    // if (!files.isEmpty()) {
    //     mVisitThread.invalidate(files.keys().toSet()); // ### not the nicest thing ever
    // }
    // // We have to invalidate before we add files back in
    // if (!files.isEmpty()) {
    //     for (QHash<Path, GccArguments>::const_iterator it = files.begin(); it != files.end(); ++it) {
    //         const GccArguments &args = it.value();
    //         if (!args.isNull()) {
    //             mParseThread.load(it.key(), args);
    //             ++added;
    //         } else if (isSource(it.key())) {
    //             qWarning() << "We don't seem to have GccArguments for" << it.key()
    //                        << mFileManager.arguments(it.key()).isNull();
    //         }
    //     }
    // }


void ParseThread::load(const Path &path)
{
    if (!mAborted) {
        ++mCount;
        QMutexLocker lock(&mMutex);
        for (File *f=mFirst; f; f = f->next) {
            if (f->path == path)
                return;
        }
        if (mLast) {
            mLast->next = new File;
            mLast = mLast->next;
        } else {
            mFirst = mLast = new File;
        }
        mLast->next = 0;
        mLast->path = path;
        mWaitCondition.wakeOne();
    }
}

struct PrecompileData
{
    QList<Path> direct;
    QList<Path> all;
};

static inline void precompileHeaders(CXFile included_file, CXSourceLocation*,
                                     unsigned include_len, CXClientData client_data)
{
    if (!include_len)
        return;

    CXString filename = clang_getFileName(included_file);

    PrecompileData* data = reinterpret_cast<PrecompileData*>(client_data);
    Path rfn = Path::resolved(clang_getCString(filename));
    if (include_len == 1)
        data->direct.append(rfn);
    data->all.append(rfn);
    clang_disposeString(filename);
}

void ParseThread::run()
{
    static const bool disablePch = getenv("RTAGS_NO_PCH");
    QVector<const char*> args;
    while (!mAborted) {
        File *f = 0;
        {
            QMutexLocker lock(&mMutex);
            if (!mFirst) {
                if (mCount)
                    qWarning("mCount shouldn't be %d, it should be 0", mCount);
                Q_ASSERT(!mCount);
                mWaitCondition.wait(&mMutex);
                if (!mFirst) {
                    Q_ASSERT(mAborted);
                    break;
                }
            }
            Q_ASSERT(mFirst);
            f = mFirst;
            mFirst = mFirst->next;
            --mCount;
            if (!mFirst) {
                mLast = 0;
                Q_ASSERT(!mCount);
            }
        }
        QElapsedTimer timer;
        timer.start();

        Q_ASSERT(f);
        GccArguments gccArguments;
        QSet<Path> dependsOn;
        mFileManager->getInfo(f->path, &gccArguments, 0, &dependsOn);
        if (gccArguments.isNull()) {
            qWarning() << "We don't seem to have GccArguments for" << f->path;
            delete f;
            continue;
        }


        bool hasChanged = (mFiles.value(f->path) != f->path.lastModified());
        if (!hasChanged) {
            foreach(const Path &header, dependsOn) {
                if (header.lastModified() != mFiles.value(header)) {
                    hasChanged = true;
                    break;
                }
            }
        }
        if (!hasChanged) {
            delete f;
            continue;
        }
        mVisitThread->invalidate(QSet<Path>() << f->path); // ### hairy
        const QList<QByteArray> compilerOptions = gccArguments.includePaths() + gccArguments.arguments("-D");
        const int compilerOptionsCount = compilerOptions.count();

        CXTranslationUnit unit = 0;
        enum { WithPCH, WithoutPCH };
        for (int i=0; i<2 && !unit; ++i) {
            PreCompile *precompile = 0;
            if (!disablePch)
                precompile = PreCompile::get(compilerOptions);
            Path pchfile;
            int argCount = compilerOptions.size();
            if (i == WithPCH) {
                if (!precompile)
                    continue;
                if (gccArguments.language() != GccArguments::LangCPlusPlus) {
                    continue;
                }
                pchfile = precompile->filename().toLocal8Bit();
                if (!pchfile.isFile()) {
                    continue;
                }
                argCount += 2;
            }

            // ### this allocates more than it needs to strictly speaking. In fact a
            // ### lot of files will have identical options so we could even reuse
            // ### the actual QVarLengthArray a lot of times.
            if (args.size() < argCount)
                args.resize(argCount);
            for (int a=0; a<compilerOptionsCount; ++a) {
                args[a] = compilerOptions.at(a).constData();
            }
            if (i == WithPCH) {
                args[compilerOptionsCount] = "-pch";
                args[compilerOptionsCount + 1] = pchfile.constData();
            }

            do {
                const time_t before = f->path.lastModified();
                // qDebug() << "parsing file" << f->path << (i == WithPCH ? "with PCH" : "without PCH");
                Q_ASSERT(!args.contains(0));
                // for (int i=0; i<argCount; ++i) {
                //     printf("%d [%s]\n", i, args.constData()[i]);
                // }

                QVector<TemporaryFile> temps = TemporaryFiles::instance()->unsavedFiles();
                unit = clang_parseTranslationUnit(mIndex, f->path.constData(),
                                                  args.constData(), argCount, temps.data(), temps.size(),
                                                  // CXTranslationUnit_NestedMacroExpansions
                                                  CXTranslationUnit_DetailedPreprocessingRecord);
                if (unit && before != f->path.lastModified())
                    continue;
            } while (false);
            if (!unit) {
                qWarning("Couldn't parse %s", f->path.constData());
                QByteArray clangLine = "clang";
                if (gccArguments.language() == GccArguments::LangCPlusPlus)
                    clangLine += "++";
                for (int j=0; j<argCount; ++j) {
                    clangLine += ' ';
                    clangLine += args.at(j);
                }
                clangLine += ' ' + f->path;
                qWarning("[%s]", clangLine.constData());
                emit parseError(f->path); // ### any way to get the parse error?
            } else {
                PrecompileData pre;
                clang_getInclusions(unit, precompileHeaders, &pre);
                // qDebug() << f->path << pre.direct << pre.all;
                if (precompile) {
                    precompile->add(pre.direct, pre.all);
                }
                mFiles[f->path] = f->path.lastModified();
                emit fileParsed(f->path, unit);
                const QSet<Path> deps = pre.all.toSet();
                foreach(const Path &dep, deps) {
                    mFiles[dep] = dep.lastModified();
                }
                if (mFileManager->addDependencies(f->path, deps))
                    emit dependenciesAdded(deps);
                qDebug() << "file was parsed" << f->path << mCount<< "left" << timer.elapsed() << "ms"
                         << (i == WithPCH ? "with PCH" : "without PCH") << compilerOptions;
            }
        }
        delete f;
    }
}
