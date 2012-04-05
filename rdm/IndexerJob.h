#ifndef IndexerJob_h
#define IndexerJob_h

#include <QtCore>
#include "Indexer.h"
#include <RTags.h>
#include "Rdm.h"

class IndexerJob : public QObject, public QRunnable
{
    Q_OBJECT;
public:
    IndexerJob(IndexerImpl* impl, int id,
               const Path& path, const Path& input,
               const QList<QByteArray>& arguments);

    int id() const { return mId; }

    void run();

    int mId;
    bool mIsPch;
    RTags::Location createLocation(CXCursor cursor);
    void addNamePermutations(CXCursor cursor, const RTags::Location &location);

    SymbolHash mSymbols;
    SymbolNameHash mSymbolNames;

    QSet<Path> mPaths;
    QHash<RTags::Location, QPair<RTags::Location, bool> > mReferences;
    Path mPath, mIn;
    QList<QByteArray> mArgs;
    DependencyHash mDependencies;
    QSet<Path> mPchDependencies;
    IndexerImpl* mImpl;
signals:
    void done(int id, const QByteArray& input);
};

#endif
