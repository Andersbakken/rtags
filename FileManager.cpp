#include "FileManager.h"

enum { CacheVersion = 1 };
FileManager::FileManager()
    : QThread(), mFilesMutex(QMutex::Recursive)
{
    setObjectName("FileManager");
    moveToThread(this);
    QSettings settings(QSettings::IniFormat, QSettings::UserScope,
                       QCoreApplication::organizationName());
    QByteArray cached = settings.value("cachedGccArguments").toByteArray();
    if (!cached.isEmpty()) {
        const int cacheVersion = settings.value("cacheVersion").toInt();
        if (cacheVersion != CacheVersion) {
            qWarning("Ignoring incompatible cache %d vs %d", cacheVersion, CacheVersion);
        } else {
            QDataStream ds(&cached, QIODevice::ReadOnly);
            ds >> mFiles;
            qDebug() << "got" << cached.size() << "of cache" << mFiles.size() << "files";
            for (QHash<Path, FileData>::const_iterator it = mFiles.begin(); it != mFiles.end(); ++it) {
                if (!it.value().arguments.isNull())
                    qDebug() << it.key() << !it.value().arguments.isNull();
            }
        }
    }
}
FileManager::~FileManager()
{
    store();
}

void FileManager::addMakefile(const Path &makefile)
{
    Q_ASSERT(makefile.exists());
    QCoreApplication::postEvent(this, new FileManagerEvent(FileManagerEvent::MakefileEvent, makefile));
}

GccArguments FileManager::arguments(const Path &path, bool *ok) const
{
    QMutexLocker lock(&mFilesMutex);
    if (ok)
        *ok = mFiles.contains(path);
    return mFiles.value(path).arguments;
}

bool FileManager::event(QEvent *event)
{
    if (event->type() == int(FileManagerEvent::MakefileEvent)) {
        const Path &path = static_cast<FileManagerEvent*>(event)->path();
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
        MakefileData data = { path, workingDir, QByteArray(), QSet<Path>(), workingDir };
        mMakefiles[proc] = data;
        proc->start(QLatin1String("make"), // some way to specify which make to use?
                    QStringList()
                    << QLatin1String("-B")
                    << QLatin1String("-n")
                    << QLatin1String("-j1")
                    << QLatin1String("-p")
                    << QLatin1String("-f")
                    << path);
        qDebug() << "addMakefile" << path;
        return true;
    }
    return QThread::event(event);
}

void FileManager::onMakeFinished(int statusCode)
{
    QProcess *proc = qobject_cast<QProcess*>(sender());
    if (statusCode != 0) {
        qWarning("make error: %s", proc->readAllStandardError().constData());
    }
    mMakefiles.remove(proc);
    proc->deleteLater();
    store();
}

void FileManager::onMakeOutput()
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
                QMutexLocker lock(&mFilesMutex);
                if (!args.hasInput() || !args.isCompile()) {
                    QStringList strings = QString::fromLocal8Bit(line).split(' ');
                    const int size = strings.size();
                    if (size >= 3 && strings.first().endsWith(":")) {
                        const Path sourceFile = Path::resolved(strings.at(1).toLocal8Bit(), data.directory);
                        if (sourceFile.isFile()) {
                            FileData &fd = mFiles[sourceFile];
                            for (int i=2; i<size; ++i) {
                                const Path header = Path::resolved(strings.at(i).toLocal8Bit(), data.directory);
                                if (header.isFile()) {
                                    mFiles[header].dependents.insert(sourceFile);
                                    fd.dependees.insert(header);
                                }
                            }
                        }
                    }

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
                        // qDebug() << "setting arguments for" << file << "to" << args.raw();
                        mFiles[file].arguments = args;
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
    
void FileManager::onMakeError(QProcess::ProcessError error)
{
    qWarning() << error << qobject_cast<QProcess*>(sender())->errorString();
}

void FileManager::store()
{
    QSettings settings(QSettings::IniFormat, QSettings::UserScope,
                       QCoreApplication::organizationName());

    QByteArray out;
    {
        QDataStream ds(&out, QIODevice::WriteOnly);
        ds << mFiles;
    }
    settings.setValue("cacheVersion", CacheVersion);
    qDebug() << "writing" << out.size() << "of cache";
    settings.setValue("cachedGccArguments", out);
}

QDataStream &operator<<(QDataStream &ds, const FileManager::FileData &fd)
{
    ds << fd.arguments << fd.dependents << fd.dependees;
    return ds;
}

QDataStream &operator>>(QDataStream &ds, FileManager::FileData &fd)
{
    ds >> fd.arguments >> fd.dependents >> fd.dependees;
    return ds;
}

bool FileManager::getInfo(const Path &path, GccArguments *args, QSet<Path> *dependents, QSet<Path> *dependees) const
{
    QMutexLocker lock(&mFilesMutex);
    const QHash<Path, FileData>::const_iterator it = mFiles.find(path);
    if (it != mFiles.end()) {
        if (args)
            *args = (*it).arguments;
        if (dependents)
            *dependents = (*it).dependents;
        if (dependees)
            *dependees = (*it).dependees;
        return true;
    }
    return false;
}
