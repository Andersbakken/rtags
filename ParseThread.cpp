#include "ParseThread.h"
#include "PreCompile.h"

static ParseThread *sParseThreadInstance = 0; // ### hack
class MakefileManager : public QObject
{
    Q_OBJECT;
public:
    MakefileManager()
        : QObject(QCoreApplication::instance())
    {
        connect(&mFileSystemWatcher, SIGNAL(fileChanged(QString)),
                this, SLOT(onFileChanged(QString)));
    }
    static MakefileManager *instance()
    {
        static QMutex mutex;
        QMutexLocker lock(&mutex);
        static MakefileManager *inst = new MakefileManager;
        return inst;
    }
public slots:
    void watchPath(const Path &path)
    {
        // qDebug() << "watchPath" << path << mWatchedFiles.contains(path);
        Q_ASSERT(path.exists());
        if (mWatchedFiles.contains(path))
            return;
        mWatchedFiles[path] = path.lastModified();
        mFileSystemWatcher.addPath(path);
    }
    void onFileChanged(const QString &path)
    {
        const Path p = path.toLocal8Bit();
        // qDebug() << "onFileChanged" << p << p.lastModified()
        //          << mWatchedFiles.value(p) << p.exists();
        if (!p.exists()) {
            mWatchedFiles.remove(p);
            mFileSystemWatcher.removePath(path);
        } else if (p.lastModified() != mWatchedFiles.value(p)) {
            mWatchedFiles[p] = p.lastModified();
            sParseThreadInstance->handleFileChanged(p);
        }
    }
    void onMakeFinished(int statusCode)
    {
        QProcess *proc = qobject_cast<QProcess*>(sender());
        if (statusCode != 0) {
            qWarning("make error: %s", proc->readAllStandardError().constData());
        }
        mMakefiles.remove(proc);
        proc->deleteLater();
    }

    void onMakeOutput()
    {
        QProcess *proc = qobject_cast<QProcess*>(sender());
        MakefileData &data = mMakefiles[proc];
        int i = data.buffer.size();
        data.buffer += proc->readAllStandardOutput();
        int last = 0;
        const int size = data.buffer.size();
        while (i < size) {
            if (data.buffer.at(i++) == '\n') {
                const QByteArray line = QByteArray::fromRawData(data.buffer.constData() + last,
                                                                i - last - 1);
                last = i;
                // printf("%s\n", line.constData());
                if (!line.isEmpty()) {
                    if (data.reject.isValid()
                        && !data.reject.isEmpty()
                        && QString::fromLocal8Bit(line).contains(data.reject)) {
                        // ### use other regexp lib
                        if (Options::s_verbose)
                            qDebug() << "rejecting" << line << data.reject.pattern();
                        continue;
                    }

                    if (data.accept.isValid()
                        && !data.accept.isEmpty()
                        && !QString::fromLocal8Bit(line).contains(data.accept)) {
                        if (Options::s_verbose)
                            qDebug() << "not accepting" << line << data.accept.pattern();
                        continue;
                    }

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
                            if (Options::s_verbose) {
                                qDebug() << "setting workingDirectory to" << p << "from"
                                         << data.workingDirectory;
                            }
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
                        // if (line.contains(".cpp")) {
                        //     qWarning("No input here or maybe this isn't a compile %d %d %s",
                        //              args.hasInput(), args.isCompile(),
                        //              line.constData());
                        //     if (args.input().size() > 1) {
                        //         qWarning() << args.input();
                        //     }
                        // }
                        continue;
                    }

                    foreach(const Path &file, args.input()) { // already resolved
                        Q_ASSERT(file.exists());
                        if (!data.seen.contains(file)) {
                            data.seen.insert(file);
                            sParseThreadInstance->addFile(file, args);
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

    void onMakeError(QProcess::ProcessError error)
    {
        qWarning() << error << qobject_cast<QProcess*>(sender())->errorString();
    }
public:
    struct MakefileData {
        Path path, directory;
        QRegExp accept, reject;
        QByteArray buffer;
        QSet<Path> seen;
        Path workingDirectory;
    };
    QHash<QProcess *, MakefileData> mMakefiles;
    QFileSystemWatcher mFileSystemWatcher;
    QHash<Path, time_t> mWatchedFiles;
};

ParseThread::ParseThread()
    : mAborted(false), mFirst(0), mLast(0), mCount(0), mIndex(clang_createIndex(1, 0))
{
    MakefileManager::instance();
    setObjectName("ParseThread");
    Q_ASSERT(!sParseThreadInstance);
    sParseThreadInstance = this;
    moveToThread(this); // we have a slot
    // ### for now
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
    Q_ASSERT(sParseThreadInstance == this);
    sParseThreadInstance = 0;
    // ### clean up
}

void ParseThread::abort()
{
    mAborted = true;
    mWaitCondition.wakeOne();
}

void ParseThread::addMakefile(const Path &path, const QRegExp &accept, const QRegExp &reject)
{
    Path workingDir = path.parentDir();
    Q_ASSERT(path.isFile() && path.isResolved() && workingDir.isDir());
    QDir::setCurrent(workingDir); // ### hmmmm
    QProcess *proc = new QProcess(MakefileManager::instance());
    proc->setWorkingDirectory(workingDir);
    // proc->moveToThread(this);
    connect(proc, SIGNAL(finished(int)), MakefileManager::instance(), SLOT(onMakeFinished(int)));
    connect(proc, SIGNAL(error(QProcess::ProcessError)), MakefileManager::instance(), SLOT(onMakeError(QProcess::ProcessError)));
    connect(proc, SIGNAL(readyReadStandardOutput()), MakefileManager::instance(), SLOT(onMakeOutput()));
    MakefileManager::MakefileData data = { path, workingDir, accept, reject, QByteArray(),
                                           QSet<Path>(), workingDir };
    MakefileManager::instance()->mMakefiles[proc] = data;
    proc->start(QLatin1String("make"), // some way to specify which make to use?
                QStringList()
                << QLatin1String("-B")
                << QLatin1String("-n")
                << QLatin1String("-f")
                // << QLatin1String("-j1") // ### why doesn't this work?
                << path);
    qDebug() << "addMakefile" << path << accept.pattern() << reject.pattern();
}

void ParseThread::addFile(const Path &path, const GccArguments &args,
                          QObject *receiver, const char *member)
{
    ++mCount;
    QMutexLocker lock(&mMutex);
    if (mLast) {
        mLast->next = new File;
        mLast = mLast->next;
    } else {
        mFirst = mLast = new File;
    }
    mLast->receiver = receiver;
    mLast->member = member;
    Q_ASSERT(!receiver == !member);
    mLast->next = 0;
    mLast->path = path;
    mLast->arguments = args.raw().isEmpty() ? mFiles.value(path).arguments : args;
    QSettings settings(QSettings::IniFormat, QSettings::UserScope,
                       QCoreApplication::organizationName());
    settings.beginGroup("GccArguments");
    const QString key = QString::fromLocal8Bit(path);
    if (mLast->arguments.raw().isEmpty()) {
        const QByteArray in = settings.value(key).toByteArray();
        if (!in.isEmpty()) {
            QDataStream ds(in);
            ds >> mLast->arguments;
        }
    } else {
        QByteArray out;
        QDataStream ds(&out, QIODevice::WriteOnly);
        ds << mLast->arguments;
        settings.setValue(key, out);
    }
    mWaitCondition.wakeOne();
}

void ParseThread::handleFileChanged(const Path &p)
{
    qDebug() << p << "was modified";

    if (p.exists() && mFiles.contains(p))
        reparse(p);
    foreach(const Path &pp, mDependencies.value(p)) {
        reparse(pp);
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
    Path::initStaticData();
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
                foreach(const Path &header, pre.all) {
                    if (!mDependencies.contains(header)) {
                        QMetaObject::invokeMethod(MakefileManager::instance(), "watchPath", Q_ARG(Path, header));
                    }
                    mDependencies[header].insert(f->path);
                }

                if (!mFiles.contains(f->path)) {
                    QMetaObject::invokeMethod(MakefileManager::instance(), "watchPath", Q_ARG(Path, f->path));
                }
                struct ParsedFileData data = { f->arguments, f->receiver, f->member };
                mFiles[f->path] = data;
                if (f->receiver) {
                    Q_ASSERT(f->member);
                    QMetaObject::invokeMethod(f->receiver, f->member, Qt::AutoConnection,
                                              Q_ARG(Path, f->path),
                                              Q_ARG(void*, unit));
                } else {
                    emit fileParsed(f->path, unit);
                }
                qDebug() << "file was parsed" << f->path << mCount<< "left" << timer.elapsed() << "ms"
                         << (i == WithPCH ? "with PCH" : "without PCH");
            }
        }
        delete f;
    }
}

void ParseThread::reparse(const Path &path)
{
    if (!mFiles.contains(path)) {
        qWarning("Can't reparse %s", path.constData());
        return;
    }
    {
        QMutexLocker lock(&mMutex);
        for (File *f = mFirst; f; f = f->next) {
            if (f->path == path) {
                qDebug() << "already queued for reparse" << path;
                return; // already in the queue, raise condition? ###
            }
        }
    }
    emit invalidated(path);
    qDebug() << "reparsing" << path;
    Q_ASSERT(mFiles.contains(path));
    const ParsedFileData &data = mFiles[path];
    addFile(path, data.arguments, data.receiver, data.member);
}

void ParseThread::loadTranslationUnit(const Path &path, QObject *receiver, const char *member)
{
    addFile(path, mFiles.value(path).arguments, receiver, member);
}

#include "ParseThread.moc"
