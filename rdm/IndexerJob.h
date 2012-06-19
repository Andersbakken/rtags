#ifndef IndexerJob_h
#define IndexerJob_h

#include <QtCore>
#include "Indexer.h"
#include <RTags.h>
#include "Rdm.h"
#include "Job.h"
#include "Str.h"
#include "AbortInterface.h"
#include <clang-c/Index.h>

class IndexerJob : public QObject, public QRunnable, public AbortInterface
{
    Q_OBJECT;
public:
    enum Flag {
        DirtyPch = 0x04,
        Dirty = 0x02,
        Makefile = 0x1, // these are used as QThreadPool priorites
        Priorities = DirtyPch|Dirty|Makefile,
        NeedsDirty = 0x010,
        FixIt = 0x020
    };
    IndexerJob(Indexer *indexer, int id, unsigned flags,
               const Path &input, const QList<ByteArray> &arguments);
    int priority() const { return mFlags & Priorities; }
    virtual void run();

    const int mId;
    const unsigned mFlags;
    bool mIsPch;
    Location createLocation(const CXCursor &cursor , bool *blocked);
    ByteArray addNamePermutations(const CXCursor &cursor, const Location &location, bool addToDb);
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
    QHash<Str, CXCursor> mHeaderHash;
    bool mDoneFullUSRScan;
    ReferenceHash mReferences;
    const Path mIn;
    const quint32 mFileId;
    const QList<ByteArray> mArgs;
    DependencyHash mDependencies;
    QSet<quint32> mPchDependencies;
    Indexer *mIndexer;
    QHash<ByteArray, Location> mPchUSRHash;

    QList<Path> mPchHeaders;
    CXTranslationUnit mUnit;
signals:
    void done(int id, const Path &path, bool isPch, const ByteArray &msg);
};

#endif
