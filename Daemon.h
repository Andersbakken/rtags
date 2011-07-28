#ifndef DAEMON_H
#define DAEMON_H

#include <QObject>
#include <QString>
#include <QStringList>
#include <QHash>
#include <QThreadPool>
#include <QFileSystemWatcher>
#include <clang-c/Index.h>
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
    Q_INVOKABLE QHash<QByteArray, QVariant> runCommand(const QHash<QByteArray, QVariant>& args);

private slots:
    void onFileChanged(const QString &path);
    void onParseError(const QByteArray &absoluteFilePath);
    void onFileParsed(const QByteArray &absoluteFilePath, void *unit);
private:
    // ### need to add a function for code completion
    QHash<QByteArray, QVariant> lookup(const QHash<QByteArray, QVariant>& args);
    QHash<QByteArray, QVariant> lookupLine(const QHash<QByteArray, QVariant>& args);
    QHash<QByteArray, QVariant> addMakefile(const QHash<QByteArray, QVariant>& args);
    QHash<QByteArray, QVariant> addSourceFile(const QHash<QByteArray, QVariant>& args);
    QHash<QByteArray, QVariant> removeSourceFile(const QHash<QByteArray, QVariant>& args);
    QHash<QByteArray, QVariant> loadAST(const QHash<QByteArray, QVariant>& args);
    QHash<QByteArray, QVariant> saveAST(const QHash<QByteArray, QVariant>& args);
    bool writeAST(const QHash<QByteArray, CXTranslationUnit>::const_iterator &it);
    bool addSourceFile(const QByteArray& absoluteFilePath,
                       unsigned options = CXTranslationUnit_CacheCompletionResults,
                       QHash<QByteArray, QVariant>* result = 0);
    void addMakefileLine(const QByteArray& line, const QByteArray &dirpath,
                         QSet<QByteArray> &seen);
    QHash<QByteArray, QVariant> fileList(const QHash<QByteArray, QVariant> &args);
    void addTranslationUnit(const QByteArray &absoluteFilePath,
                            unsigned options = 0,
                            const QList<QByteArray> &compilerOptions = QList<QByteArray>());
private:
    QThreadPool m_threadPool;
    CXIndex m_index;
    QHash<QByteArray, CXTranslationUnit> m_translationUnits;
    QFileSystemWatcher m_fileSystemWatcher;
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
