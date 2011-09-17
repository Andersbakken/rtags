#ifndef VisitThread_h
#define VisitThread_h

#include <QtCore>
#include "Path.h"
#include "Location.h"

struct Node;
struct Match
{
    Match(uint nodeTypes)
        : nodeTypes(nodeTypes)
    {}
    virtual ~Match() {}

    enum MatchResult {
        Finish,
        Recurse,
        Skip
    };
    // path means e.g. namespace::class:: (including trailing double colons)
    virtual MatchResult match(const QByteArray &path, const Node *node) = 0;

    const uint nodeTypes;
};

struct CursorNode;
class VisitThread : public QThread
{
    Q_OBJECT
public:
    VisitThread();
    void lookup(Match *match);
    void printTree();
    QSet<Path> files() const;
    void abort();
public slots:
    void invalidate(const QSet<Path> &paths);
    void onFileParsed(const Path &path, void *unit);
private:
    void buildTree(Node *node, CursorNode *c, QList<CursorNode*> &references);
    void addReference(CursorNode *c);

    mutable QMutex mMutex;
    Node *mRoot;
    QHash<QByteArray, Node*> mNodes;
    int mBytes;
    bool mQuitting;
};

#endif
