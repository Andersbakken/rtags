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
    virtual void run();

    int mId;
    bool mIsPch;
    Location createLocation(CXCursor cursor, bool *blocked);
    QByteArray addNamePermutations(CXCursor cursor, const Location &location, bool addToDb);
    static CXChildVisitResult indexVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data);
    static void inclusionVisitor(CXFile included_file, CXSourceLocation* include_stack,
                                 unsigned include_len, CXClientData client_data);

    struct Cursor {
        CXCursor cursor;
        Location location;
        CXCursorKind kind;
    };

    CXChildVisitResult processCursor(const Cursor &cursor, const Cursor &ref);
    Cursor findByUSR(CXCursor cursor);

    QList<Cursor> mDelayed;
    SymbolHash mSymbols;
    SymbolNameHash mSymbolNames;

    enum PathState {
        Unset,
        Index,
        DontIndex
    };
    QHash<quint32, PathState> mPaths;
    ReferenceHash mReferences;
    Path mIn;
    QList<QByteArray> mArgs;
    DependencyHash mDependencies;
    QSet<quint32> mPchDependencies;
    Indexer *mIndexer;
    QHash<QByteArray, Location> mPchUSRHash;

    QHash<QByteArray, CXCursor> mHeaderHash;
    QSet<quint32> mHeaderHashFiles;
    QList<Path> mPchHeaders;
signals:
    void done(int id, const Path &path, bool isPch, const QByteArray &msg);
};

#endif
