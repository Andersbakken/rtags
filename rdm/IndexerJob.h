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
    IndexerJob(Indexer *indexer, int id, Indexer::IndexType type, const Path &input,
               const QList<QByteArray> &arguments);
    int priority() const { return mType; }
    virtual void run();

    const int mId;
    const Indexer::IndexType mType;
    bool mIsPch;
    Location createLocation(const CXCursor &cursor , bool *blocked);
    QByteArray addNamePermutations(const CXCursor &cursor, const Location &location, bool addToDb);
    static CXChildVisitResult indexVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data);
    static void inclusionVisitor(CXFile included_file, CXSourceLocation *include_stack,
                                 unsigned include_len, CXClientData client_data);

    struct Cursor {
        CXCursor cursor;
        Location location;
        CXCursorKind kind;
    };

    CXChildVisitResult processCursor(const Cursor &cursor, const Cursor &ref);
    Cursor findByUSR(const CXCursor &cursor, CXCursorKind kind, const Location &loc);

    QList<Cursor> mDelayed;
    SymbolHash mSymbols;
    SymbolNameHash mSymbolNames;

    enum PathState {
        Unset,
        Index,
        DontIndex
    };
    QHash<quint32, PathState> mPaths;
    QHash<QByteArray, CXCursor> mHeaderHash;
    bool mDoneFullUSRScan;
    ReferenceHash mReferences;
    const Path mIn;
    const quint32 mFileId;
    const QList<QByteArray> mArgs;
    DependencyHash mDependencies;
    QSet<quint32> mPchDependencies;
    Indexer *mIndexer;
    QHash<QByteArray, Location> mPchUSRHash;

    QList<Path> mPchHeaders;
signals:
    void done(int id, const Path &path, bool isPch, const QByteArray &msg);
};

#endif
