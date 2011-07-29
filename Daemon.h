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
#ifdef EBUS_ENABLED
#include <QtNetwork>
#endif

struct Location {
    Location()
        : line(0), column(0)
    {}
    Location(CXCursor cursor)
        : line(0), column(0)
    {
        CXSourceLocation location = clang_getCursorLocation(cursor);
        CXFile file;
        clang_getInstantiationLocation(location, &file, &line, &column, 0);
        fileName = eatString(clang_getFileName(file));
        if (fileName.isEmpty()) { // This seems to happen
            line = column = 0 ;
        } else {
            resolvePath(fileName);
        }
        Q_ASSERT(fileName.isEmpty() == (line == 0 && column == 0));
    }

    bool exists() const
    {
        return fileExists(fileName);
    }

    QByteArray fileName;
    unsigned line, column;
};
static inline QDebug operator<<(QDebug dbg, const Location &loc)
{
    if (!loc.exists()) {
        dbg << "Location(null)";
    } else {
        dbg << QString("Location(%1:%2:%3)").
            arg(QString::fromLocal8Bit(loc.fileName)).arg(loc.line).arg(loc.column);
    }
    return dbg;
}

struct Node
{
    Node *parent, *nextSibling, *firstChild; // doubly-linked?
    CXString symbolName;
    enum Type {
        Root,
        MethodDeclaration,
        MethodDefinition,
        Class,
        Struct,
        Namespace,
        MethodCall, // Reference?
        VariableDeclaration,
        VariableReference
    } type;
    Location location;
    uint hash;

    Node();
    Node(Node *p, CXCursor c, const Location &l, uint hash);
    ~Node();
    QByteArray toString() const;
    void print() const;
    static const char *typeToName(Type type);
};

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
    Node *m_root;
    QHash<unsigned, Node*> m_nodes;
    struct PendingReference {
        CXCursor cursor;
        CXCursor reference;
        Location location;
        unsigned hash;
    };
    QList<PendingReference> m_pendingReferences;
    Node *createOrGet(CXCursor cursor);
    static CXChildVisitResult buildTree(CXCursor cursor, CXCursor, CXClientData data);

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
