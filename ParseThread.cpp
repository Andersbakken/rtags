#include "ParseThread.h"
#include "PreCompile.h"
#include "FileManager.h"

ParseThread::ParseThread()
    : mAborted(false), mFirst(0), mLast(0), mCount(0), mIndex(clang_createIndex(1, 0))
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

void ParseThread::load(const Path &path)
{
    ++mCount;
    QMutexLocker lock(&mMutex);
    if (mLast) {
        mLast->next = new File;
        mLast = mLast->next;
    } else {
        mFirst = mLast = new File;
    }
    mLast->next = 0;
    mLast->path = path;
    mLast->arguments = FileManager::instance()->arguments(path);
    mWaitCondition.wakeOne();
}

void ParseThread::handleFileChanged(const Path &p)
{
    // qDebug() << p << "was modified";

    // if (p.exists() && mFiles.contains(p))
    //     reparse(p);
    // foreach(const Path &pp, mDependencies.value(p)) {
    //     reparse(pp);
    // }
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
        const QList<QByteArray> compilerOptions = f->arguments.includePaths() + f->arguments.arguments("-D");
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
                if (f->arguments.language() != GccArguments::LangCPlusPlus) {
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

                unit = clang_parseTranslationUnit(mIndex, f->path.constData(),
                                                  args.constData(), argCount, 0, 0,
                                                  0); // ### for options?
                if (unit && before != f->path.lastModified())
                    continue;
            } while (false);
            if (!unit) {
                qWarning("Couldn't parse %s", f->path.constData());
                QByteArray clangLine = "clang";
                if (f->arguments.language() == GccArguments::LangCPlusPlus)
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
                if (precompile) {
                    precompile->add(pre.direct, pre.all);
                }
                emit fileParsed(f->path, unit);
                // }
                qDebug() << "file was parsed" << f->path << mCount<< "left" << timer.elapsed() << "ms"
                         << (i == WithPCH ? "with PCH" : "without PCH");
            }
        }
        delete f;
    }
}

void ParseThread::reparse(const Path &path)
{
    // if (!mFiles.contains(path)) {
    //     qWarning("Can't reparse %s", path.constData());
    //     return;
    // }
    // {
    //     QMutexLocker lock(&mMutex);
    //     for (File *f = mFirst; f; f = f->next) {
    //         if (f->path == path) {
    //             qDebug() << "already queued for reparse" << path;
    //             return; // already in the queue, raise condition? ###
    //         }
    //     }
    // }
    // emit invalidated(path);
    // qDebug() << "reparsing" << path;
    // Q_ASSERT(mFiles.contains(path));
    // const ParsedFileData &data = mFiles[path];
    // addFile(path, data.arguments, data.receiver, data.member);
}
