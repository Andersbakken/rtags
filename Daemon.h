#ifndef DAEMON_H
#define DAEMON_H

#include <QObject>
#include <QString>
#include <QStringList>
#include <QHash>
#include <QThreadPool>
#include <QFileSystemWatcher>
#include <clang-c/Index.h>
#include "Utils.h"
#include "GccArguments.h"
#include "Path.h"
#include "Location.h"
#include "ParseThread.h"
#include "VisitThread.h"
#include "FileManager.h"

struct Node;
class Daemon : public QObject
{
    Q_OBJECT;
public:
    Daemon(QObject* parent = 0);
    ~Daemon();

    QHash<QByteArray, QVariant> runCommand(const QHash<QByteArray, QVariant>& dashArgs,
                                           QList<QByteArray> freeArgs);
public slots:
    void onDependenciesAdded(const QSet<Path> &path);
    void quit();
private:
    // ### need to add a function for code completion
    QHash<QByteArray, QVariant> lookup(const QHash<QByteArray, QVariant>& args,
                                       const QList<QByteArray> &freeArgs);
    QHash<QByteArray, QVariant> printTree(const QHash<QByteArray, QVariant>& args,
                                          const QList<QByteArray> &freeArgs);
    QHash<QByteArray, QVariant> followSymbol(const QHash<QByteArray, QVariant>& args,
                                             const QList<QByteArray> &freeArgs);
    QHash<QByteArray, QVariant> addMakefile(const QHash<QByteArray, QVariant>& dashArgs,
                                            const QList<QByteArray>& freeArgs);
    QHash<QByteArray, QVariant> load(const QHash<QByteArray, QVariant>&,
                                     const QList<QByteArray> &freeArgs);
    void addDeps(const Path &path, QHash<Path, GccArguments> &deps, QSet<Path> &seen);
private:
    ParseThread mParseThread;
    VisitThread mVisitThread;
    FileManager mFileManager;
};

#endif
