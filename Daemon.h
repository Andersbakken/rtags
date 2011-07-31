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
#ifdef EBUS_ENABLED
#include "EBus.h"
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
    QByteArray symbolName;
    enum Type {
        None = 0x000000,
        Root = 0x000001,
        MethodDeclaration = 0x000002,
        MethodDefinition = 0x000004,
        MethodReference = 0x000008, // Reference?
        Class = 0x000010,
        Struct = 0x000020,
        Namespace = 0x000040,
        VariableDeclaration = 0x000080,
        VariableReference = 0x000100,
        Enum = 0x000200,
        EnumValue = 0x000400,
        All = (None|Root|MethodDeclaration|MethodDefinition|MethodReference|Class|
               Struct|Namespace|VariableDeclaration|VariableReference|Enum|EnumValue)
    } type;
    Location location;
    uint hash;

    Node();
    Node(Node *p, CXCursor c, const Location &l, uint hash);
    ~Node();
    QByteArray toString() const;
    void print() const;
    static const char *typeToName(Type type, bool abbrev = false);
};

class Daemon : public QObject
{
    Q_OBJECT
public:
    Daemon(QObject* parent = 0);
    ~Daemon();

    bool start();
    Q_INVOKABLE QHash<QByteArray, QVariant> runCommand(const QHash<QByteArray, QVariant>& dashArgs,
                                                       const QList<QByteArray>& freeArgs);

private slots:
    void onFileChanged(const QString &path);
    void onParseError(const QByteArray &absoluteFilePath);
    void onFileParsed(const QByteArray &absoluteFilePath, const QList<QByteArray> &options, void *unit);
private:
    // ### need to add a function for code completion
    QHash<QByteArray, QVariant> lookup(const QHash<QByteArray, QVariant>& args);
    QHash<QByteArray, QVariant> lookupLine(const QHash<QByteArray, QVariant>& args);
    QHash<QByteArray, QVariant> addMakefile(const QHash<QByteArray, QVariant>& dashArgs, const QList<QByteArray>& freeArgs);
    QHash<QByteArray, QVariant> addSourceFile(const QHash<QByteArray, QVariant>& args);
    QHash<QByteArray, QVariant> removeSourceFile(const QHash<QByteArray, QVariant>& args);
    QHash<QByteArray, QVariant> loadAST(const QHash<QByteArray, QVariant>& args);
    QHash<QByteArray, QVariant> saveAST(const QHash<QByteArray, QVariant>& args);
    bool writeAST(const QHash<QByteArray, CXTranslationUnit>::const_iterator &it);
    bool addSourceFile(const QByteArray& absoluteFilePath,
                       unsigned options = CXTranslationUnit_CacheCompletionResults,
                       QHash<QByteArray, QVariant>* result = 0);
    bool addMakefileLine(const QByteArray& line, const QByteArray &dirpath,
                         QSet<QByteArray> &seen);
    QHash<QByteArray, QVariant> fileList(const QHash<QByteArray, QVariant> &args);
    void addTranslationUnit(const QByteArray &absoluteFilePath,
                            const GccArguments &args,
                            unsigned options = 0);
    void reparseFile(const QByteArray &absoluteFilePath);
    void removeReferences(const QByteArray &absoluteFilePath);
private:
    QThreadPool m_threadPool;
    CXIndex m_index;
    QHash<QByteArray, GccArguments> m_files;
    QHash<QByteArray, QSet<QByteArray> > m_dependencies;
    QFileSystemWatcher m_fileSystemWatcher;
    Node *m_root;
    QReadWriteLock m_readWriteLock;
    QHash<unsigned, Node*> m_nodes;
    struct PendingReference {
        CXCursor cursor;
        CXCursor reference;
        Location location;
    };
    QHash<uint, PendingReference> m_pendingReferences;
    Node *createOrGet(CXCursor cursor);
    static CXChildVisitResult buildTree(CXCursor cursor, CXCursor, CXClientData data);
    int m_pendingTranslationUnits;

#ifdef EBUS_ENABLED
    EBusDaemon m_ebus;

private slots:
    void ebusConnected(EBus* ebus);
    void ebusDataReady();
#endif

};

#endif
