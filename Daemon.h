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
#ifdef EBUS_ENABLED
#include "EBus.h"
#endif

struct Node;
class Daemon : public QObject
{
    Q_OBJECT
    public:
    Daemon(QObject* parent = 0);
    ~Daemon();

    bool start();
    Q_INVOKABLE QHash<QByteArray, QVariant> runCommand(const QHash<QByteArray, QVariant>& dashArgs,
                                                       const QList<QByteArray>& freeArgs);
public slots:
    void onFileParsed(const Path &path, void *translationUnit);
private:
    // ### need to add a function for code completion
    QHash<QByteArray, QVariant> complete(const QHash<QByteArray, QVariant>& args,
                                         const QList<QByteArray> &freeArgs);
    QHash<QByteArray, QVariant> lookup(const QHash<QByteArray, QVariant>& args,
                                       const QList<QByteArray> &freeArgs);
    QHash<QByteArray, QVariant> lookupLine(const QHash<QByteArray, QVariant>& args,
                                           const QList<QByteArray> &freeArgs);
    QHash<QByteArray, QVariant> addMakefile(const QHash<QByteArray, QVariant>& dashArgs,
                                            const QList<QByteArray>& freeArgs);
    QHash<QByteArray, QVariant> load(const QHash<QByteArray, QVariant>& args,
                                     const QList<QByteArray> &freeArgs);
    QHash<QByteArray, QVariant> fileList(const QHash<QByteArray, QVariant> &args,
                                         const QList<QByteArray> &freeArgs);
    bool writeAST(const QHash<Path, CXTranslationUnit>::const_iterator it);
private:
    ParseThread mParseThread;
    VisitThread mVisitThread;
    QHash<Path, CXTranslationUnit> mTranslationUnits;
#ifdef EBUS_ENABLED
    EBusDaemon mEbus;
private slots:
    void ebusConnected(EBus* ebus);
    void ebusDataReady();
#endif

};

#endif
