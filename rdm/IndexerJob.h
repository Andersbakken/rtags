#ifndef IndexerJob_h
#define IndexerJob_h

#include <QtCore>
#include "Indexer.h"
#include <RTags.h>
#include "Rdm.h"
#include "Job.h"
#include "AbortInterface.h"
#include <clang-c/Index.h>

class IndexerJob : public QObject, public QRunnable, public AbortInterface
{
    Q_OBJECT;
public:
    IndexerJob(Indexer* indexer, int id, const Path& input, const QList<QByteArray>& arguments);
    ~IndexerJob();
    void run();

    int mId;
    bool mIsPch;
    Location createLocation(CXCursor cursor);
    QByteArray addNamePermutations(CXCursor cursor, const Location &location, bool addToDb);
    static CXChildVisitResult indexVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data);
    static void inclusionVisitor(CXFile included_file, CXSourceLocation* include_stack,
                                 unsigned include_len, CXClientData client_data);

    SymbolHash mSymbols;
    SymbolNameHash mSymbolNames;

    QSet<Path> mPaths;
    ReferenceHash mReferences;
    Path mIn;
    QList<QByteArray> mArgs;
    DependencyHash mDependencies;
    QSet<Path> mPchDependencies;
    Indexer *mIndexer;
    QHash<QByteArray, Location> mPchUSRHash;
    QList<Path> mPchHeaders;
    CXIndex mIndex;
    CXTranslationUnit mUnit;

signals:
    void done(int id, const Path &path, bool isPch, const QByteArray &msg);
};

#endif
