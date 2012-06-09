#include "IndexerJob.h"
#include "SHA256.h"
#include "MemoryMonitor.h"
#include "Server.h"

static inline QList<Path> extractPchFiles(const QList<QByteArray> &args)
{
    QList<Path> out;
    bool nextIsPch = false;
    foreach (const QByteArray &arg, args) {
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

IndexerJob::IndexerJob(Indexer *indexer, int id, unsigned flags,
                       const Path &input, const QList<QByteArray> &arguments)
    : mId(id), mFlags(flags), mIsPch(false), mDoneFullUSRScan(false), mIn(input),
      mFileId(Location::insertFile(input)), mArgs(arguments), mIndexer(indexer),
      mPchHeaders(extractPchFiles(arguments)), mUnit(0)
{
    setAutoDelete(false);
}

static inline quint32 fileId(CXFile file)
{
    return Location(file, 0).fileId();
}

void IndexerJob::inclusionVisitor(CXFile includedFile,
                                  CXSourceLocation *includeStack,
                                  unsigned includeLen,
                                  CXClientData userData)
{
    IndexerJob *job = static_cast<IndexerJob*>(userData);
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
    } else if (!path.isSystem()) {
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
    QByteArray ret;
    QByteArray qname;
    QByteArray qparam, qnoparam;

    CXCursor cur = cursor, null = clang_getNullCursor();
    CXCursorKind kind;
    bool first = true;
    for (;;) {
        if (clang_equalCursors(cur, null))
            break;
        kind = clang_getCursorKind(cur);
        if (!first) {
            bool ok = false;
            switch (kind) {
            case CXCursor_Namespace:
            case CXCursor_ClassDecl:
            case CXCursor_StructDecl:
            case CXCursor_CXXMethod:
            case CXCursor_Constructor:
            case CXCursor_FunctionDecl:
                ok = true;
                break;
            default:
                break;
            }
            if (!ok)
                break;
        }

        CXStringScope displayName(clang_getCursorDisplayName(cur));
        const char *name = clang_getCString(displayName.string);
        if (!name || !strlen(name)) {
            break;
        }
        qname = QByteArray(name);
        if (ret.isEmpty()) {
            ret = qname;
            if (!addToDB)
                return ret;
        }
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

        if (first) {
            first = false;
            switch (kind) {
            case CXCursor_Namespace:
            case CXCursor_ClassDecl:
            case CXCursor_StructDecl:
            case CXCursor_CXXMethod:
            case CXCursor_Constructor:
            case CXCursor_FunctionDecl:
            case CXCursor_VarDecl:
            case CXCursor_ParmDecl:
                break;
            default:
                // these don't need the scope
                return ret;
            }
        }

        cur = clang_getCursorSemanticParent(cur);
    }
    return ret;
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

CXChildVisitResult IndexerJob::indexVisitor(CXCursor cursor,
                                            CXCursor /*parent*/,
                                            CXClientData client_data)
{
    IndexerJob *job = static_cast<IndexerJob*>(client_data);
    if (job->isAborted())
        return CXChildVisit_Break;

    if (testLog(Debug))
        debug() << "indexVisitor" << cursor << clang_getCursorReferenced(cursor);
    const CXCursorKind kind = clang_getCursorKind(cursor);
    if (!isInteresting(kind))
        return CXChildVisit_Recurse;

    bool blocked = false;
    const Location loc = job->createLocation(cursor, &blocked);
    if (blocked) {
        switch (kind) {
        case CXCursor_FunctionDecl:
        case CXCursor_CXXMethod:
        case CXCursor_Destructor:
        case CXCursor_Constructor:
            job->mHeaderHash[clang_getCursorUSR(cursor)] = cursor;
            break;
        case CXCursor_ClassDecl:
        case CXCursor_StructDecl:
        case CXCursor_Namespace:
            return CXChildVisit_Recurse;
        default:
            break;
        }
        return CXChildVisit_Continue;
    } else if (loc.isNull()) {
        return CXChildVisit_Recurse;
    }
    CXCursor ref = clang_getCursorReferenced(cursor);
    const CXCursorKind refKind = clang_getCursorKind(ref);
    // the kind won't change even if the reference is looked up from elsewhere

    /* CXCursor_CallExpr is the right thing to use for invocations of constructors */
    if (kind == CXCursor_CallExpr && (refKind == CXCursor_CXXMethod || refKind == CXCursor_FunctionDecl))
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
                if (testLog(Debug)) {
                    debug() << "Looked up definition for ref" << ref << cursor;
                }
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
    if (testLog(Debug))
        debug() << "processCursor" << cursor.cursor << ref.cursor;

    if (cursor.kind == CXCursor_InclusionDirective) {
        CXFile includedFile = clang_getIncludedFile(cursor.cursor);
        if (includedFile) {
            const Location refLoc(includedFile, 0);
            if (!refLoc.isNull()) {
                CXSourceRange range = clang_getCursorExtent(cursor.cursor);
                unsigned int end;
                clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &end);
                unsigned tokenCount = 0;
                CXToken *tokens = 0;
                clang_tokenize(mUnit, range, &tokens, &tokenCount);
                CursorInfo &info = mSymbols[cursor.location];
                info.target = refLoc;
                info.kind = cursor.kind;
                info.isDefinition = false;
                info.symbolLength = end - cursor.location.offset();
                for (unsigned i=0; i<tokenCount; ++i) {
                    if (clang_getTokenKind(tokens[i]) == CXToken_Literal) {
                        CXStringScope scope(clang_getTokenSpelling(mUnit, tokens[i]));
                        info.symbolName = "#include ";
                        info.symbolName += clang_getCString(scope.string);
                        mSymbolNames[info.symbolName].insert(cursor.location);
                        break;
                    }
                }
                if (tokens) {
                    clang_disposeTokens(mUnit, tokens, tokenCount);
                }
            }
        }
        return CXChildVisit_Recurse;
    }

    CursorInfo &info = mSymbols[cursor.location];
    if (!info.symbolLength) {
        if (mIsPch) {
            const QByteArray usr = Rdm::eatString(clang_getCursorUSR(cursor.cursor));
            if (!usr.isEmpty()) {
                mPchUSRHash[usr] = cursor.location;
            }
        }
        info.isDefinition = clang_isCursorDefinition(cursor.cursor);
        info.kind = cursor.kind;
        CXString name;
        const bool isReference = Rdm::isReference(info.kind);

        if (isReference) {
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
        info.symbolName = addNamePermutations(cursor.cursor, cursor.location, !isReference);
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
        headerHash.clear();
        if (unit) {
            clang_disposeTranslationUnit(unit);
            unit = 0;
        }
        if (index) {
            clang_disposeIndex(index);
            index = 0;
        }
    }

    QHash<Str, CXCursor> &headerHash;
    CXTranslationUnit &unit;
    CXIndex &index;
};
void IndexerJob::run()
{
    QElapsedTimer timer;
    timer.start();
    int elapsed = timer.elapsed();
    if (elapsed > 10) {
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
    foreach (const QByteArray &arg, mArgs) {
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
    mUnit = clang_parseTranslationUnit(index, mIn.constData(),
                                       clangArgs.data(), idx, 0, 0,
                                       CXTranslationUnit_Incomplete | CXTranslationUnit_DetailedPreprocessingRecord);
    Scope scope = { mHeaderHash, mUnit, index };
    const time_t timeStamp = time(0);
    warning() << "loading unit" << clangLine << (mUnit != 0);
    if (isAborted()) {
        return;
    }

    mDependencies[mFileId].insert(mFileId);
    if (!mUnit) {
        error() << "got 0 unit for" << clangLine;
        mIndexer->addDependencies(mDependencies);
        FileInformation fi;
        fi.compileArgs = mArgs;
        fi.lastTouched = timeStamp;

        Rdm::writeFileInformation(mFileId, mArgs, timeStamp);
    } else {
        clang_getInclusions(mUnit, inclusionVisitor, this);

        clang_visitChildren(clang_getTranslationUnitCursor(mUnit), indexVisitor, this);
        if (mIsPch) {
            Q_ASSERT(!pchName.isEmpty());
            if (clang_saveTranslationUnit(mUnit, pchName.constData(), clang_defaultSaveOptions(mUnit)) != CXSaveError_None) {
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
            if (mFlags & NeedsDirty) {
#ifdef QT_DEBUG
                for (QHash<quint32, PathState>::const_iterator it = mPaths.begin(); it != mPaths.end(); ++it) {
                    if (it.value() == Index && it.key() != mFileId && !Location::path(it.value()).isSystem()) {
                        // ideally system headers would have ended up in mVisitedFiles on startup
                        error("This file should not have been dirty %s %d", Location::path(it.key()).constData(), it.key());
                    }
                }
#endif
                Rdm::dirty(QSet<quint32>() << mFileId);
            }
            Rdm::writeSymbols(mSymbols, mReferences, mFileId);
            Rdm::writeSymbolNames(mSymbolNames);
            Rdm::writeFileInformation(mFileId, mArgs, timeStamp);
            if (mIsPch)
                mIndexer->setPchDependencies(mIn, mPchDependencies);
        }

    }
    char buf[1024];
    const char *strings[] = { "", "(pch)", "(dirty)", "(pch, dirty)" };
    enum {
        None = 0x0,
        Pch = 0x1,
        Dirty = 0x2
    };
    const int w = snprintf(buf, sizeof(buf), "Visited %s in %lldms.%s (%d syms, %d refs, %d deps, %d symNames)%s",
                           mIn.constData(), timer.elapsed(),
                           qPrintable(waitingForPch ? QString(" Waited for pch: %1ms.").arg(waitingForPch)
                                      : QString()),
                           mSymbols.size(), mReferences.size(), mDependencies.size(), mSymbolNames.size(),
                           strings[(mPchHeaders.isEmpty() ? None : Pch) | (mFlags & NeedsDirty ? Dirty : None)]);

    emit done(mId, mIn, mIsPch, QByteArray(buf, w));
    if (testLog(Warning)) {
        warning() << "We're using" << double(MemoryMonitor::usage()) / double(1024 * 1024) << "MB of memory" << elapsed << "ms";
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
    bool ok = false;
    switch (kind) {
    case CXCursor_FunctionDecl:
        ok = (clang_isCursorDefinition(cursor) && loc.fileId() == mFileId);
        break;
    case CXCursor_CXXMethod:
    case CXCursor_Destructor:
    case CXCursor_Constructor:
        ok = (clang_isCursorDefinition(cursor) && loc.fileId() == mFileId && !isInline(cursor));
        break;
    default:
        break;
    }
    if (!ok) {
        const Cursor ret = { clang_getNullCursor(), Location(), CXCursor_FirstInvalid };
        return ret;
    }

    const Str usr(clang_getCursorUSR(cursor));
    if (!usr.length()) {
        const Cursor ret = { clang_getNullCursor(), Location(), CXCursor_FirstInvalid };
        return ret;
    }

    const QByteArray key = QByteArray::fromRawData(usr.data(), usr.length());
    Location refLoc = mPchUSRHash.value(key);
    if (!refLoc.isNull()) {
        const Cursor ret = { cursor, refLoc, clang_getCursorKind(cursor) };
        // ### even if this isn't the right CXCursor it's good enough for our needs
        return ret;
    }

    QHash<Str, CXCursor>::const_iterator it = mHeaderHash.find(usr);
    if (it != mHeaderHash.end()) {
        const CXCursor ref = it.value();
        const Cursor ret = { ref, createLocation(ref, 0), clang_getCursorKind(ref) };
        assert(!clang_equalCursors(ref, cursor)); // ### why is this happening?
        return ret;
    }
    const Cursor ret = { clang_getNullCursor(), Location(), CXCursor_FirstInvalid };
    return ret;
}
