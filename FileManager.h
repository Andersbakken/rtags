#ifndef FileManager_h
#define FileManager_h

#include <QtCore>
#include "Path.h"
#include "GccArguments.h"

class ParseThread;
class FileManager : public QObject
{
    Q_OBJECT;
public:
    FileManager(ParseThread *parseThread);
    ~FileManager();
    void addMakefile(const Path &makefile);
    GccArguments arguments(const Path &path, bool *ok = 0) const;
    void getInfo(const Path &path, GccArguments *args,
                 QSet<Path> *dependents, QSet<Path> *dependsOn) const;
    QByteArray dependencyMap() const;
    bool addDependencies(const Path &source, const QSet<Path> &headers);
    bool isDone() const;
signals:
    void done();
private slots:
    void onMakeFinished(int statusCode);
    void onMakeOutput();
    void onMakeError(QProcess::ProcessError error);
private:
    struct MakefileData {
        Path path, directory;
        QByteArray buffer;
        QSet<Path> seen;
        Path workingDirectory;
    };
    QHash<QProcess *, MakefileData> mMakefiles;

    struct FileData {
        GccArguments arguments;
        QSet<Path> dependents;
        QSet<Path> dependsOn;
        // If this is Foo.cpp, dependsOn contains Foo.h,
        // If this is Foo.h, dependents contains Foo.cpp
    };
    friend QDataStream &operator<<(QDataStream &ds, const FileData &fd);
    friend QDataStream &operator>>(QDataStream &ds, FileData &fd);

    QHash<Path, FileData> mFiles;
    ParseThread *mParseThread;
};

#endif
