#ifndef ClangRunnable_h
#define ClangRunnable_h

#include <QtCore>
#include "GccArguments.h"
#include "Path.h"
#include "Location.h"

struct Node;
struct CursorNode;
class ClangRunnable : public QObject, public QRunnable
{
    Q_OBJECT
public:
    static void init();
    static void cleanup();
    ClangRunnable(const Path &file, const GccArguments &args);
    void run();
    static bool save(const QByteArray &file);
signals:
    void finished();
private:
    struct PendingReference {
        CursorNode *node;
        Location location;
    };
    void buildTree(Node *node, CursorNode *c, QHash<QByteArray, PendingReference> &references);
    void addReference(CursorNode *c, const QByteArray &id, const Location &location);
    

    const Path mFile;
    const GccArguments mArgs;
    static QMutex sPchMutex;
    static QMutex sTreeMutex;
    static Node *sRoot;
    static int32_t sLongestId;


};


#endif
