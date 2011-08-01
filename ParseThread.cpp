#include "ParseThread.h"
#include "PreCompile.h"

static ParseThread *sParseThreadInstance = 0; // ### hack
class MakefileManager : public QObject
{
    Q_OBJECT;
public:
    MakefileManager()
        : QObject(QCoreApplication::instance())
    {}
    static MakefileManager *instance()
    {
        static MakefileManager *inst = new MakefileManager;
        return inst;
    }
public slots:
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
            if (data.buffer.at(i) == '\n') {
                const QByteArray line = data.buffer.mid(last, i - last);
                if (!line.isEmpty()) {
                    if (data.reject.isValid() && !data.reject.isEmpty() && QString::fromLocal8Bit(line).contains(data.reject)) { // ### use other regexp lib
                        if (Options::s_verbose)
                            qDebug() << "rejecting" << line << data.reject.pattern();
                    } else if (data.accept.isValid() && !data.accept.isEmpty() && !QString::fromLocal8Bit(line).contains(data.accept)) {
                        if (Options::s_verbose)
                            qDebug() << "not accepting" << line << data.accept.pattern();
                    } else {
                        GccArguments args;
                        if (args.parse(line, data.directory) && args.hasInput() && args.isCompile()) {
                            foreach(Path file, args.input()) {
                                if (!file.resolve()) {
                                    qWarning("%s doesn't exist", file.constData());
                                } else if (!data.seen.contains(file)) {
                                    data.seen.insert(file);
                                    sParseThreadInstance->addFile(file, args);
                                }
                            }
                        } else if (!args.errorString().isEmpty()) {
                            qWarning("Can't parse line %s (%s)", line.constData(), qPrintable(args.errorString()));
                        }

                    }

                }
                last = i + 1;
            }
            ++i;
        }
        if (i < size) {
            data.buffer.remove(0, last);
        } else {
            data.buffer.clear();
        }
    }
    
    void onMakeError(QProcess::ProcessError error)
    {
        qDebug() << error << qobject_cast<QProcess*>(sender())->errorString();
    }
public:
    struct MakefileData {
        Path path, directory;
        QRegExp accept, reject;
        QByteArray buffer;
        QSet<Path> seen;
    };
    QHash<QProcess *, MakefileData> mMakefiles;
};
#include "ParseThread.moc"

ParseThread::ParseThread()
    : mAborted(false), mFirst(0), mLast(0), mCount(0), mIndex(clang_createIndex(1, 0))
{
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
    MakefileManager::MakefileData data = { path, workingDir, accept, reject, QByteArray(), QSet<Path>() };
    MakefileManager::instance()->mMakefiles[proc] = data;
    proc->start(QLatin1String("make"), // some way to specify which make to use?
                QStringList()
                << QLatin1String("-B")
                << QLatin1String("-n")
                << QLatin1String("-f")
                << path);
    qDebug() << "addMakefile" << path << accept << reject;
}

void ParseThread::addFile(const Path &path, const GccArguments &args, QObject *receiver, const char *member)
{
    qDebug() << "adding file" << path << ++mCount;
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
    mLast->arguments = args;
    qDebug() << "Waking up thread";
    mWaitCondition.wakeOne();
}

void ParseThread::onFileChanged(const QString &path)
{
    Path p = path.toLocal8Bit();
    emit invalidated(p);
    foreach(const Path &pp, mDependencies.value(p)) {
        emit invalidated(pp);
    }
    if (p.exists() && mFiles.contains(p)) {
        reparse(p);
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
    Path::initStaticData();
    QFileSystemWatcher watcher;
    connect(&watcher, SIGNAL(fileChanged(QString)), this, SLOT(onFileChanged(QString)));
    QVector<const char*> args;
    while (!mAborted) {
        File *f = 0;
        {
            QMutexLocker lock(&mMutex);
            if (!mFirst) {
                qDebug() << "Waiting because !mFirst";
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

        PreCompile* precompile = PreCompile::get(compilerOptions);
        const QByteArray pchfile = precompile->filename().toLocal8Bit();

        // ### this allocates more than it needs to strictly speaking. In fact a
        // ### lot of files will have identical options so we could even reuse
        // ### the actual QVarLengthArray a lot of times.
        const int size = compilerOptions.size();
        const int extra = pchfile.isEmpty() ? 0 : 2;
        if (args.size() < size + extra)
            args.resize(size + extra);
        for (int i=0; i<size; ++i) {
            args[i] = compilerOptions.at(i).constData();
        }

        if (extra) {
            args[size] = "-include-pch";
            args[size + 1] = pchfile.constData();
        }

        time_t before;
        CXTranslationUnit unit = 0;
        do {
            if (unit) {
                clang_disposeTranslationUnit(unit);
                qDebug() << "file was modified underneath us" << before << f->path.lastModified();
            }
            
            before = f->path.lastModified();
            qDebug() << "parsing file" << f->path;
            unit = clang_parseTranslationUnit(mIndex, f->path.constData(),
                                              args.constData(), size + extra, 0, 0,
                                              0); // ### for options?
        } while (unit && before != f->path.lastModified());
        if (!unit) {
            qWarning("Couldn't parse %s", f->path.constData());
            emit parseError(f->path); // ### any way to get the parse error?
        } else {
            PrecompileData pre;
            clang_getInclusions(unit, precompileHeaders, &pre);
            precompile->add(pre.direct, pre.all);
            foreach(const Path &header, pre.all) {
                if (!mDependencies.contains(header))
                    watcher.addPath(header);
                mDependencies[header].insert(f->path);
            }
            
            if (mFiles.contains(f->path)) {
                mFiles[f->path] = f->arguments;
            } else {
                watcher.addPath(f->path);
            }
            if (f->receiver) {
                Q_ASSERT(f->member);
                QMetaObject::invokeMethod(f->receiver, f->member, Qt::AutoConnection,
                                          Q_ARG(Path, f->path),
                                          Q_ARG(void*, unit));
            } else {
                emit fileParsed(f->path, unit);
            }
            qDebug() << "file was parsed" << f->path << mCount<< "left" << timer.elapsed() << "ms";
        }
        delete f;
    }
}

void ParseThread::reparse(const Path &path)
{
    Q_ASSERT(mFiles.contains(path));
    addFile(path, mFiles.value(path));
}

void ParseThread::loadTranslationUnit(const Path &path, QObject *receiver, const char *member)
{
    Q_ASSERT(mFiles.contains(path));
    addFile(path, mFiles.value(path), receiver, member);
}
