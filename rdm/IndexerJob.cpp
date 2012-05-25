#include "IndexerJob.h"
#include "SHA256.h"
#include "MemoryMonitor.h"
#include "Server.h"

static inline QList<Path> extractPchFiles(const QList<QByteArray>& args)
{
    QList<Path> out;
    bool nextIsPch = false;
    foreach (const QByteArray& arg, args) {
        if (arg.isEmpty())
            continue;

        if (nextIsPch) {
            nextIsPch = false;
            out.append(arg);
        } else if (arg == "-include-pch") {
            nextIsPch = true;
        }
    }
    return out;
}

// static int count = 0;
// static int active = 0;

IndexerJob::IndexerJob(Indexer* indexer, int id, const Path& input, const QList<QByteArray>& arguments)
    : mId(id), mIsPch(false), mIn(input), mFileId(Location::insertFile(input)), mArgs(arguments),
      mIndexer(indexer), mPchHeaders(extractPchFiles(arguments))
{
    // qDebug() << metaObject()->className() << "born" << ++count << ++active;
    setAutoDelete(false);
}

static inline quint32 fileId(CXFile file)
{
    return Location(file, 0).fileId();
}

void IndexerJob::inclusionVisitor(CXFile includedFile,
                                  CXSourceLocation* includeStack,
                                  unsigned includeLen,
                                  CXClientData userData)
{
    IndexerJob* job = static_cast<IndexerJob*>(userData);
    if (job->isAborted())
        return;
    const Location l(includedFile, 0);

    const Path path = l.path();
    job->mSymbolNames[path].insert(l);
    const char *fn = path.fileName();
    // since path comes from Location::sIdsToPaths it will never go away so this is safe
    job->mSymbolNames[QByteArray::fromRawData(fn, strlen(fn))].insert(l);

    const quint32 fileId = l.fileId();
    if (!includeLen) {
        job->mDependencies[fileId].insert(fileId);
        if (job->mIsPch)
            job->mPchDependencies.insert(fileId);
    } else if (!Rdm::isSystem(path)) {
        for (unsigned i=0; i<includeLen; ++i) {
            CXFile originatingFile;
            clang_getSpellingLocation(includeStack[i], &originatingFile, 0, 0, 0);
            Location loc(originatingFile, 0);
            const quint32 f = loc.fileId();
            // qDebug() << i << includeLen << job->mIn << Location(originatingFile, 0).path();
            if (f)
                job->mDependencies[fileId].insert(f);
        }
        if (job->mIsPch) {
            job->mPchDependencies.insert(fileId);
        }
    }
}

QByteArray IndexerJob::addNamePermutations(const CXCursor &cursor, const Location &location, bool addToDB)
{
    QByteArray qname;
    QByteArray qparam, qnoparam;

    CXString displayName;
    CXCursor cur = cursor, null = clang_getNullCursor();
    CXCursorKind kind;
    for (;;) {
        if (clang_equalCursors(cur, null))
            break;
        kind = clang_getCursorKind(cur);
        if (clang_isTranslationUnit(kind))
            break;

        displayName = clang_getCursorDisplayName(cur);
        const char* name = clang_getCString(displayName);
        if (!name || !strlen(name)) {
            clang_disposeString(displayName);
            break;
        }
        qname = QByteArray(name);
        if (qparam.isEmpty()) {
            qparam.prepend(qname);
            if (addToDB) {
                qnoparam.prepend(qname);
                const int sp = qnoparam.indexOf('(');
                if (sp != -1)
                    qnoparam = qnoparam.left(sp);
            }
        } else {
            qparam.prepend(qname + "::");
            if (addToDB)
                qnoparam.prepend(qname + "::");
        }
        Q_ASSERT(!qparam.isEmpty());
        if (addToDB) {
            mSymbolNames[qparam].insert(location);
            if (qparam != qnoparam) {
                Q_ASSERT(!qnoparam.isEmpty());
                mSymbolNames[qnoparam].insert(location);
            }
        }

        clang_disposeString(displayName);
        cur = clang_getCursorSemanticParent(cur);
    }
    return qparam;
}

Location IndexerJob::createLocation(const CXCursor &cursor, bool *blocked)
{
    CXSourceLocation location = clang_getCursorLocation(cursor);
    Location ret;
    if (!clang_equalLocations(location, clang_getNullLocation())) {
        CXFile file;
        unsigned start;
        clang_getSpellingLocation(location, &file, 0, 0, &start);
        if (file) {
            ret = Location(file, start);
            if (blocked) {
                const quint32 fileId = ret.fileId();
                PathState &state = mPaths[fileId];
                if (state == Unset) {
                    state = mIndexer->visitFile(fileId, mIn) ? Index : DontIndex;
                }
                if (state == DontIndex) {
                    *blocked = true;
                    // qDebug() << "ignored" << Rdm::cursorToString(cursor) << "for" << mIn;
                    return Location();
                }
                *blocked = false;
            }
        }
    }
    return ret;
}

static inline bool isInteresting(CXCursorKind kind)
{
    if (clang_isInvalid(kind))
        return false;
    switch (kind) {
    case CXCursor_CXXThisExpr:
    case CXCursor_CXXTypeidExpr:
    case CXCursor_CXXStaticCastExpr:
    case CXCursor_CXXNullPtrLiteralExpr:
    case CXCursor_CXXNewExpr: // ### Are these right?
    case CXCursor_CXXDeleteExpr:
    case CXCursor_CompoundAssignOperator: // ### Are these right?
    case CXCursor_CompoundStmt:
    case CXCursor_ParenExpr:
    case CXCursor_StringLiteral:
    case CXCursor_IntegerLiteral:
    case CXCursor_InitListExpr:
    case CXCursor_BreakStmt:
    case CXCursor_DefaultStmt:
    case CXCursor_BinaryOperator:
    case CXCursor_CaseStmt:
    case CXCursor_ConditionalOperator:
    case CXCursor_CStyleCastExpr:
    case CXCursor_ForStmt:
    case CXCursor_WhileStmt:
    case CXCursor_DoStmt:
    case CXCursor_IfStmt:
    case CXCursor_CXXBoolLiteralExpr:
    case CXCursor_CharacterLiteral:
    case CXCursor_UnaryOperator:
    case CXCursor_ReturnStmt:
    case CXCursor_CXXAccessSpecifier:
    case CXCursor_CXXConstCastExpr:
    case CXCursor_CXXDynamicCastExpr:
    case CXCursor_CXXReinterpretCastExpr:
        return false;
    default:
        break;
    }
    return true;
}

CXChildVisitResult findReferenceVisitor(CXCursor cursor, CXCursor, CXClientData u)
{
    switch (clang_getCursorKind(cursor)) {
    case CXCursor_CXXMethod:
    case CXCursor_Destructor:
    case CXCursor_Constructor: {
        QHash<QByteArray, CXCursor> &headerHash = *reinterpret_cast<QHash<QByteArray, CXCursor> *>(u);
        CXStringScope usr(clang_getCursorUSR(cursor));
        const char *cstr = clang_getCString(usr.string);
        headerHash[cstr] = cursor;
        break; }
    default:
        break;
    }
    return CXChildVisit_Continue;
}

CXChildVisitResult IndexerJob::indexVisitor(CXCursor cursor,
                                            CXCursor /*parent*/,
                                            CXClientData client_data)
{
    IndexerJob* job = static_cast<IndexerJob*>(client_data);
    if (job->isAborted())
        return CXChildVisit_Break;

    const CXCursorKind kind = clang_getCursorKind(cursor);
    if (!isInteresting(kind))
        return CXChildVisit_Recurse;

    bool blocked = false;
    const Location loc = job->createLocation(cursor, &blocked);
    if (blocked) {
        return CXChildVisit_Continue;
    } else if (loc.isNull()) {
        return CXChildVisit_Recurse;
    }
    CXCursor ref = clang_getCursorReferenced(cursor);
    const CXCursorKind refKind = clang_getCursorKind(ref);
    // the kind won't change even if the reference is looked up from elsewhere
    if (kind == CXCursor_CallExpr && refKind == CXCursor_CXXMethod)
        return CXChildVisit_Recurse;

    const Cursor c = { cursor, loc, kind };
    Location refLoc;
    if (!clang_equalCursors(cursor, ref)) {
        refLoc = job->createLocation(ref, 0);
    } else {
        if (!clang_isCursorDefinition(cursor)) {
            ref = clang_getCursorDefinition(cursor);
            if (!clang_equalCursors(clang_getNullCursor(), ref)) {
                Q_ASSERT(!clang_equalCursors(cursor, ref));
                refLoc = job->createLocation(ref, 0);
            }
        }
        if (refLoc.isNull()) {
            const Cursor r = job->findByUSR(cursor, kind, loc);
            if (r.kind != CXCursor_FirstInvalid)
                return job->processCursor(c, r);
        }
    }
    const Cursor r = { ref, refLoc, refKind };
    return job->processCursor(c, r);
}

CXChildVisitResult IndexerJob::processCursor(const Cursor &cursor, const Cursor &ref)
{
    CursorInfo &info = mSymbols[cursor.location];
    if (!info.symbolLength) {
        if (mIsPch) {
            const QByteArray usr = Rdm::eatString(clang_getCursorUSR(cursor.cursor));
            if (!usr.isEmpty()) {
                mPchUSRHash[usr] = cursor.location;
            }
        }
        info.kind = cursor.kind;
        CXString name;
        if (clang_isReference(cursor.kind)) {
            name = clang_getCursorSpelling(ref.cursor);
        } else {
            name = clang_getCursorSpelling(cursor.cursor);
        }
        const char *cstr = clang_getCString(name);
        info.symbolLength = cstr ? strlen(cstr) : 0;
        clang_disposeString(name);
        if (!info.symbolLength) {
            mSymbols.remove(cursor.location);
            return CXChildVisit_Recurse;
        }
        const bool addToDB = (clang_isCursorDefinition(cursor.cursor)
                              || cursor.kind == CXCursor_FunctionDecl);
        info.symbolName = addNamePermutations(cursor.cursor, cursor.location, addToDB);
    } else if (info.kind == CXCursor_Constructor && cursor.kind == CXCursor_TypeRef) {
        return CXChildVisit_Recurse;
    }

    if (!clang_isInvalid(ref.kind) && !ref.location.isNull() && ref.location != cursor.location) {
        info.target = ref.location;
        Rdm::ReferenceType referenceType = Rdm::NormalReference;
        if (ref.kind == cursor.kind) {
            switch (ref.kind) {
            case CXCursor_Constructor:
            case CXCursor_Destructor:
            case CXCursor_CXXMethod:
                referenceType = Rdm::MemberFunction;
                break;
            case CXCursor_FunctionDecl:
                referenceType = Rdm::GlobalFunction;
                break;
            default:
                break;
            }
        }
        mReferences[cursor.location] = qMakePair(ref.location, referenceType);
    } else if (cursor.kind == CXCursor_InclusionDirective) {
        CXFile includedFile = clang_getIncludedFile(cursor.cursor);
        Location refLoc(includedFile, 0);
        if (!refLoc.isNull()) {
            info.target = ref.location;
            mReferences[cursor.location] = qMakePair(ref.location, Rdm::NormalReference);
        }
    }
    return CXChildVisit_Recurse;
}

static QByteArray pchFileName(const QByteArray &header)
{
    return Server::pchDir() + SHA256::hash(header.constData());
}

struct Scope {
    ~Scope()
    {
        cleanup();
    }
    void cleanup()
    {
        if (unit) {
            clang_disposeTranslationUnit(unit);
            unit = 0;
        }
        if (index) {
            clang_disposeIndex(index);
            index = 0;
        }
    }


    CXTranslationUnit &unit;
    CXIndex &index;
};
void IndexerJob::run()
{
    QElapsedTimer timer;
    timer.start();
    int elapsed = timer.elapsed();
    if (elapsed) {
        error() << mIn << "waited for" << elapsed << "ms";
    }
    // while (!Rdm::waitForMemory(10000)) {
    //     error("%s Waiting for rdm to shrink", mIn.constData());
    // }
    if (!mPchHeaders.isEmpty())
        mPchUSRHash = mIndexer->pchUSRHash(mPchHeaders);
    const quint64 waitingForPch = timer.restart();

    QVarLengthArray<const char*, 32> clangArgs(mArgs.size());
    QByteArray clangLine = "clang ";
    bool nextIsPch = false, nextIsX = false;
    QByteArray pchName;

    QList<Path> pchFiles;
    int idx = 0;
    foreach (const QByteArray& arg, mArgs) {
        if (arg.isEmpty())
            continue;

        if (nextIsPch) {
            nextIsPch = false;
            pchFiles.append(pchFileName(arg));
            clangArgs[idx++] = pchFiles.last().constData();
            clangLine += pchFiles.last().constData();
            clangLine += " ";
            continue;
        }

        if (nextIsX) {
            nextIsX = false;
            mIsPch = (arg == "c++-header" || arg == "c-header");
        }
        clangArgs[idx++] = arg.constData();
        clangLine += arg;
        clangLine += " ";
        if (arg == "-include-pch") {
            nextIsPch = true;
        } else if (arg == "-x") {
            nextIsX = true;
        }
    }
    if (mIsPch) {
        pchName = pchFileName(mIn);
    }
    clangLine += mIn;

    if (isAborted()) {
        return;
    }
    CXIndex index = clang_createIndex(1, 1);
    CXTranslationUnit unit = clang_parseTranslationUnit(index, mIn.constData(),
                                                        clangArgs.data(), idx, 0, 0,
                                                        CXTranslationUnit_Incomplete | CXTranslationUnit_DetailedPreprocessingRecord);
    Scope scope = { unit, index };
    const time_t timeStamp = time(0);
    warning() << "loading unit" << clangLine << (unit != 0);
    if (isAborted()) {
        return;
    }

    mDependencies[mFileId].insert(mFileId);
    if (!unit) {
        error() << "got 0 unit for" << clangLine;
        mIndexer->addDependencies(mDependencies);
        FileInformation fi;
        fi.compileArgs = mArgs;
        fi.lastTouched = timeStamp;

        Rdm::writeFileInformation(mFileId, mArgs, timeStamp);
    } else {
        clang_getInclusions(unit, inclusionVisitor, this);
        // for (QHash<quint32, QSet<quint32> >::const_iterator it = mDependencies.begin(); it != mDependencies.end(); ++it) {
        //     QList<Path> out;
        //     foreach(quint32 p, it.value()) {
        //         out.append(Location::path(p));
        //     }
        //     qDebug() << Location::path(it.key()) << "->" << out;
        // }

        clang_visitChildren(clang_getTranslationUnitCursor(unit), indexVisitor, this);
        if (mIsPch) {
            Q_ASSERT(!pchName.isEmpty());
            if (clang_saveTranslationUnit(unit, pchName.constData(), clang_defaultSaveOptions(unit)) != CXSaveError_None) {
                error() << "Couldn't save pch file" << mIn << pchName;
            } else {
                mIndexer->setPchUSRHash(mIn, mPchUSRHash);
            }
        }
        foreach(const Path &pchHeader, mPchHeaders) {
            foreach(quint32 dep, mIndexer->pchDependencies(pchHeader)) {
                mDependencies[dep].insert(mFileId);
            }
        }
        mIndexer->addDependencies(mDependencies);
        Q_ASSERT(mDependencies[mFileId].contains(mFileId));
        scope.cleanup();

        if (!isAborted()) {
            Rdm::writeSymbols(mSymbols, mReferences);
            Rdm::writeSymbolNames(mSymbolNames);
            Rdm::writeFileInformation(mFileId, mArgs, timeStamp);
            if (mIsPch)
                mIndexer->setPchDependencies(mIn, mPchDependencies);
        }

    }
    char buf[1024];
    const int w = snprintf(buf, sizeof(buf) - 1, "Visited %s in %lldms.%s (%d syms, %d refs, %d deps, %d symNames)",
                           mIn.constData(), timer.elapsed(),
                           qPrintable(waitingForPch ? QString(" Waited for pch: %1ms.").arg(waitingForPch)
                                      : QString()),
                           mSymbols.size(), mReferences.size(), mDependencies.size(), mSymbolNames.size());

    emit done(mId, mIn, mIsPch, QByteArray(buf, w));
    if (testLog(Warning)) {
        error() << "We're using" << double(MemoryMonitor::usage()) / double(1024 * 1024) << "MB of memory" << elapsed << "ms";
    }
}

CXChildVisitResult isInlineVisitor(CXCursor, CXCursor, CXClientData u)
{
    *reinterpret_cast<bool*>(u) = true;
    return CXChildVisit_Break;
}

static inline bool isInline(const CXCursor &cursor)
{
    switch (clang_getCursorKind(clang_getCursorLexicalParent(cursor))) {
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
        return true;
    default:
        return false;
    }
}

IndexerJob::Cursor IndexerJob::findByUSR(const CXCursor &cursor, CXCursorKind kind, const Location &loc)
{
    CXStringScope scope(clang_getCursorUSR(cursor));
    const char *cstr = clang_getCString(scope.string);
    if (!cstr) {
        const Cursor ret = { clang_getNullCursor(), Location(), CXCursor_FirstInvalid };
        return ret;
    }

    const QByteArray key = QByteArray::fromRawData(cstr, strlen(cstr));
    Location refLoc = mPchUSRHash.value(key);
    if (!refLoc.isNull()) {
        const Cursor ret = { cursor, refLoc, clang_getCursorKind(cursor) };
        // ### even if this isn't the right CXCursor it's good enough for our needs
        return ret;
    }
    switch (kind) {
    case CXCursor_CXXMethod:
    case CXCursor_Destructor:
    case CXCursor_Constructor:
        // case CXCursor_FunctionDecl: // these mostly fail
        if (clang_isCursorDefinition(cursor) && loc.fileId() == mFileId && !isInline(cursor))
            break;
        // fall through
    default:
        const Cursor ret = { clang_getNullCursor(), Location(), CXCursor_FirstInvalid };
        return ret;
    }

    QHash<QByteArray, CXCursor>::const_iterator it = mHeaderHash.find(key);
    if (it == mHeaderHash.end()) {
        CXCursor parent = clang_getCursorSemanticParent(cursor);
        if (!clang_isInvalid(clang_getCursorKind(parent))) {
            clang_visitChildren(parent, findReferenceVisitor, &mHeaderHash);
            it = mHeaderHash.find(key);
        }
    }

    if (it != mHeaderHash.end()) {
        const CXCursor ref = it.value();
        const Cursor ret = { ref, createLocation(ref, 0), clang_getCursorKind(ref) };
        Q_ASSERT(!clang_equalCursors(ref, cursor));
        return ret;
    }

    const Cursor ret = { clang_getNullCursor(), Location(), CXCursor_FirstInvalid };
    return ret;
}
