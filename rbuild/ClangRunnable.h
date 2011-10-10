#ifndef ClangRunnable_h
#define ClangRunnable_h

#include <QtCore>
#include "GccArguments.h"
#include "Path.h"
#include "Location.h"
#include "Shared.h"
#include "ClangArgs.h"

struct Node;
struct CursorNode;
struct MMapData;
class ClangRunnable : public QObject, public QRunnable
{
    Q_OBJECT
public:
    static void init();
    static void cleanup();
    ClangRunnable(const Path &file, const ClangArgs &args);
    void run();
    static bool save(const QByteArray &file);
    static int initTree(const MMapData *data, const QSet<Path> &modifiedPaths);
    static int processTranslationUnit(const Path &file, CXTranslationUnit unit);
signals:
    void finished();
private:
    static int initTree(const MMapData *data, const QSet<Path> &modifiedPaths,
                        Node *node, const NodeData &nodeData,
                        QHash<Node*, QByteArray> &pendingContainingFunctions);

    struct FileData {
        ClangArgs arguments;
        int64_t lastModified;
        QHash<Path, int64_t> dependencies;
    };

    const Path mFile;
    const ClangArgs mArgs;
    static QMutex sTreeMutex;
    static Node *sRoot;
    static QMutex sFilesMutex;
    static QHash<Path, FileData> sFiles;

    friend QDataStream& operator<<(QDataStream& stream, const ClangRunnable::FileData& args);
    friend QDataStream& operator>>(QDataStream& stream, ClangRunnable::FileData& args);
    friend class RBuild;
};


#endif
