#ifndef FileManager_h
#define FileManager_h

class Makefile : public QObject
{
    Q_OBJECT
public:
    Path path;
    QRegExp reject, accept;

    virtual void handle(const Path &path, const GccArguments &args) = 0;
};

class FileManagerEvent : public QEvent
{
public:
    enum {
        WatchPathEvent = User + 1,
        MakefileEvent
    };
    FileManagerEvent(const Path &path)
        : QEvent(static_cast<QEvent::Type>(WatchPathEvent)), mPath(path), mMakefile(0)
    {
    }

    FileManagerEvent(Makefile *makefile)
        : QEvent(static_cast<QEvent::Type>(MakefileEvent)), mMakefile(makefile)
    {
    }

    const Path &path() const { return mPath; }
    Makefile *makefile() const { return mMakefile; }
private:
    const Path mPath;
    Makefile *const mMakefile;
};


class FileManager : public QThread
{
    Q_OBJECT;
public:
    FileManager()
        : QThread(), mFileSystemWatcher(0)
    {
        moveToThread(this);
    }
    static FileManager *instance()
    {
        static QMutex mutex;
        static QWeakPointer<FileManager> ptr;
        if (!ptr) {
            ptr = new FileManager;
        }
        return ptr.data();
    }

    void watchPath(const Path &path)
    {
        QCoreApplication::postEvent(this, new FileManagerEvent(path));
    }

    void addMakefile(Makefile *makefile)
    {
        Q_ASSERT(makefile);
        QCoreApplication::postEvent(this, new FileManagerEvent(makefile));
    }
signals:
    void fileChanged(const Path &path);
protected:
    void run()
    {
        QFileSystemWatcher fileSystemWatcher;
        mFileSystemWatcher = &fileSystemWatcher;
        connect(&fileSystemWatcher, SIGNAL(fileChanged(QString)),
                this, SLOT(onFileChanged(QString)));
        exec();
        mFileSystemWatcher = 0; // ### protect?
    }

    bool event(QEvent *event)
    {
        switch (event->type()) {
        case FileManagerEvent::WatchPathEvent: {
            const Path &path = static_cast<FileManagerEvent*>(event)->path();
            if (mWatchedFiles.contains(path) || !path.exists())
                return true;
            mWatchedFiles[path] = path.lastModified();
            mFileSystemWatcher->addPath(path);
            return true; }
        case FileManagerEvent::MakefileEvent: {
            Makefile *makefile = static_cast<FileManagerEvent*>(event)->makefile();
            Q_ASSERT(makefile);
            const Path &path = makefile->path;
            const QRegExp &accept = makefile->accept;
            const QRegExp &reject = makefile->reject;

            const Path workingDir = path.parentDir();
            Q_ASSERT((path.isFile() && path.isResolved() && workingDir.isDir()) || !path.exists());
            if (!path.exists())
                return true;
            QDir::setCurrent(workingDir); // ### hmmmm
            QProcess *proc = new QProcess(this);
            proc->setWorkingDirectory(workingDir);
            // proc->moveToThread(this);
            connect(proc, SIGNAL(finished(int)), this, SLOT(onMakeFinished(int)));
            connect(proc, SIGNAL(error(QProcess::ProcessError)), this, SLOT(onMakeError(QProcess::ProcessError)));
            connect(proc, SIGNAL(readyReadStandardOutput()), this, SLOT(onMakeOutput()));
            MakefileData data = { path, workingDir, accept, reject, QByteArray(), QSet<Path>(), workingDir };
            mMakefiles[proc] = data;
            proc->start(QLatin1String("make"), // some way to specify which make to use?
                        QStringList()
                        << QLatin1String("-B")
                        << QLatin1String("-n")
                        << QLatin1String("-f")
                        // << QLatin1String("-j1") // ### why doesn't this work?
                        << path);
            qDebug() << "addMakefile" << path << accept.pattern() << reject.pattern();
            return true; }
        default:
            break;
        }
        return QThread::event(event);
    }
private slots:
    void onFileChanged(const QString &path)
    {
        const Path p = path.toLocal8Bit();
        // qDebug() << "onFileChanged" << p << p.lastModified()
        //          << mWatchedFiles.value(p) << p.exists();
        if (!p.exists()) {
            mWatchedFiles.remove(p);
            mFileSystemWatcher->removePath(path);
        } else if (p.lastModified() != mWatchedFiles.value(p)) {
            mWatchedFiles[p] = p.lastModified();
            emit fileChanged(p);
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
                            // sParseThreadInstance->addFile(file, args);
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
    QFileSystemWatcher *mFileSystemWatcher;
    QHash<Path, time_t> mWatchedFiles;
};

#endif
