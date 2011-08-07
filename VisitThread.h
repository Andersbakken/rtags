#ifndef VisitThread_h
#define VisitThread_h

#include <QtCore>
#include "Path.h"
#include "Location.h"

struct Node;
typedef void (*HandleResult)(const Node *node, const QByteArray &qualifiedSymbolName, void *userData);
class VisitThread : public QThread
{
    Q_OBJECT
public:
    VisitThread();
    enum LookupFlags {
        None = 0x0,
        RegExp = 0x1
    };
    int lookup(const QList<QByteArray> &patterns, uint flags, uint nodeTypes, HandleResult handler, void *userdata);
    void printTree();
    QSet<Path> files() const;
protected:
    virtual void run();
public slots:
    void invalidate(const Path &path);
    void onFileParsed(const Path &path, void *unit);
private:
    Node *createOrGet(CXCursor cursor);
    static CXChildVisitResult buildTree(CXCursor cursor, CXCursor, CXClientData data);
    struct PendingReference {
        CXCursor cursor;
        CXCursor reference;
        Location location;
    };
    mutable QMutex mMutex;
    QSet<Path> mFiles;
    QHash<uint, PendingReference> mPendingReferences;
    Node *mRoot;
    QReadWriteLock mLock;
    QHash<unsigned, Node*> mNodes;
    int mBytes;
};

#endif
