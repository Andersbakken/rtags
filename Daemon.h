#ifndef DAEMON_H
#define DAEMON_H

#include <QObject>
#include <QString>
#include <QStringList>
#include <QHash>
#include <QFileSystemWatcher>
#include <QVariantMap>
#include <clang-c/Index.h>
#include "ThreadPool.h"
#ifdef EBUS_ENABLED
#include <QtNetwork>
#endif

class Daemon : public QObject
{
    Q_OBJECT;
public:
    Daemon(QObject* parent = 0);
    ~Daemon();

    bool start();
    Q_INVOKABLE QVariantMap runCommand(const QVariantMap& args);

private slots:
    void onFileChanged(const QString &path);
    void onParseError(const QByteArray &absoluteFilePath);
    void onFileParsed(const QByteArray &absoluteFilePath, void *unit);
private:
    // ### need to add a function for code completion
    QVariantMap lookup(const QVariantMap& args);
    QVariantMap lookupLine(const QVariantMap& args);
    QVariantMap addMakefile(const QByteArray& path, const QVariantMap& args);
    QVariantMap addSourceFile(const QVariantMap& args);
    QVariantMap removeSourceFile(const QVariantMap& args);
    QVariantMap loadAST(const QVariantMap& args);
    QVariantMap saveAST(const QVariantMap& args);
    bool writeAST(const QHash<QByteArray, CXTranslationUnit>::const_iterator &it);
    bool addSourceFile(const QByteArray& absoluteFilePath,
                       unsigned options = CXTranslationUnit_CacheCompletionResults,
                       QVariantMap* result = 0);
    bool addMakefileLine(const QList<QByteArray>& line);
    QVariantMap fileList(const QVariantMap &args);
    void addTranslationUnit(const QByteArray &absoluteFilePath,
                            unsigned options = 0,
                            const QList<QByteArray> &compilerOptions = QList<QByteArray>());
private:
    CXIndex m_index;
    QHash<QByteArray, CXTranslationUnit> m_translationUnits;
    QFileSystemWatcher m_fileSystemWatcher;
    ThreadPool m_threadPool;
#ifdef EBUS_ENABLED
    QTcpServer *m_server;
    QHash<QTcpSocket*, qint16> m_connections;

    void read(QTcpSocket *socket);
    Q_SLOT void onNewConnection();
    Q_SLOT void onReadyRead();
    Q_SLOT void onDisconnected();
#endif

};

#endif
