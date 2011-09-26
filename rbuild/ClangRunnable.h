#ifndef ClangRunnable_h
#define ClangRunnable_h

#include <QtCore>
#include "GccArguments.h"
#include "Path.h"
#include "Location.h"
#include "Shared.h"

struct Node;
struct CursorNode;
struct MMapData;
class ClangRunnable : public QObject, public QRunnable
{
    Q_OBJECT
public:
    static void init();
    static void cleanup();
    ClangRunnable(const Path &file, const GccArguments &args);
    void run();
    static bool save(const QByteArray &file);
    static void initTree(const MMapData *data, const QSet<Path> &modifiedPaths);
signals:
    void finished();
private:
    static void initTree(const MMapData *data, const QSet<Path> &modifiedPaths,
                         Node *node, const NodeData &nodeData);

    struct PendingReference {
        CursorNode *node;
        Location location;
    };
    void buildTree(Node *node, CursorNode *c, QHash<QByteArray, PendingReference> &references);
    void addReference(CursorNode *c, const QByteArray &id, const Location &location);

    struct FileData {
        GccArguments arguments;
        int64_t lastModified;
        QHash<Path, int64_t> dependencies;
    };

    const Path mFile;
    const GccArguments mArgs;
    static QMutex sPchMutex;
    static QMutex sTreeMutex;
    static Node *sRoot;
    static QHash<Path, FileData> sFiles;

    friend QDataStream& operator<<(QDataStream& stream, const ClangRunnable::FileData& args);
    friend QDataStream& operator>>(QDataStream& stream, ClangRunnable::FileData& args);
    friend class RBuild;
};


#endif
