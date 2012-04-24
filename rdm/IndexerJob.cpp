#include "IndexerJob.h"
#include "SHA256.h"
#include "DependencyEvent.h"
#include "IndexerSyncer.h"

IndexerJob::IndexerJob(Indexer* indexer, int id,
                       const Path& path, const Path& input,
                       const QList<QByteArray>& arguments)
    : Job(id), mIsPch(false), mPath(path), mIn(input), mArgs(arguments), mIndexer(indexer),
      mAborted(false)
{
}

void IndexerJob::abort()
{
    mAborted = true;
    mIndexer->mCondition.wakeAll();
}

static void inclusionVisitor(CXFile included_file,
                             CXSourceLocation* include_stack,
                             unsigned include_len,
                             CXClientData client_data)
{
    (void)include_len;
    (void)included_file;
    IndexerJob* job = static_cast<IndexerJob*>(client_data);
    if (job->mAborted)
        return;
    CXString fn = clang_getFileName(included_file);
    const char *cstr = clang_getCString(fn);
    // ### make this configurable
    if ((strncmp("/usr/", cstr, 5) != 0)
        || (strncmp("/usr/home/", cstr, 10) == 0)) {
        Path path = Path::canonicalized(cstr);
        foreach (const QByteArray& arg, job->mIndexer->defaultArgs()) {
            if (arg.contains(path)) {
                clang_disposeString(fn);
                return;
            }
        }
        for (unsigned i=0; i<include_len; ++i) {
            CXFile originatingFile;
            clang_getSpellingLocation(include_stack[i], &originatingFile, 0, 0, 0);
            CXString originatingFn = clang_getFileName(originatingFile);
            job->mDependencies[path].insert(Path::canonicalized(clang_getCString(originatingFn)));
            clang_disposeString(originatingFn);
        }
        if (!include_len) {
            job->mDependencies[path].insert(path);
        }
        if (job->mIsPch) {
            job->mPchDependencies.insert(path);
        }
    }
    clang_disposeString(fn);
}

void IndexerJob::addNamePermutations(CXCursor cursor, const RTags::Location &location)
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
            qnoparam.prepend(qname);
            const int sp = qnoparam.indexOf('(');
            if (sp != -1)
                qnoparam = qnoparam.left(sp);
        } else {
            qparam.prepend(qname + "::");
            qnoparam.prepend(qname + "::");
        }
        Q_ASSERT(!qparam.isEmpty());
        mSymbolNames[qparam].insert(location);
        if (qparam != qnoparam) {
            Q_ASSERT(!qnoparam.isEmpty());
            mSymbolNames[qnoparam].insert(location);
        }

        clang_disposeString(displayName);
        cur = clang_getCursorSemanticParent(cur);
    }
}

RTags::Location IndexerJob::createLocation(CXCursor cursor)
{
    CXSourceLocation location = clang_getCursorLocation(cursor);
    RTags::Location ret;
    if (!clang_equalLocations(location, clang_getNullLocation())) {
        CXFile file;
        unsigned start;
        clang_getSpellingLocation(location, &file, 0, 0, &start);
        CXString fn = clang_getFileName(file);
        const char *fileName = clang_getCString(fn);
        if (fileName && strlen(fileName)) {
            ret.path = fileName;
            ret.path.canonicalize(); // ### could canonicalize directly
            ret.offset = start;
            mPaths.insert(ret.path);
        }
        // unsigned l, c;
        // clang_getSpellingLocation(location, 0, &l, &c, 0);
        // QByteArray out;
        // out.append(ret.path);
        // out.append(':');
        // out.append(QByteArray::number(l));
        // out.append(':');
        // out.append(QByteArray::number(c));
        // debug() << ret.key() << "is" << out;
        clang_disposeString(fn);
    }
    return ret;
}

static CXChildVisitResult indexVisitor(CXCursor cursor,
                                       CXCursor /*parent*/,
                                       CXClientData client_data)
{
    IndexerJob* job = static_cast<IndexerJob*>(client_data);
    if (job->mAborted)
        return CXChildVisit_Break;

// #ifdef QT_DEBUG
//     {
//         CXCursor ref = clang_getCursorReferenced(cursor);
//         if (clang_equalCursors(cursor, ref) && !clang_isCursorDefinition(ref)) {
//             ref = clang_getCursorDefinition(ref);
//         }
//         RTags::Location loc = job->createLocation(cursor);
//         RTags::Location rloc = job->createLocation(ref);
//         if (Rdm::cursorToString(cursor).contains("canonicalizePath")
//             || Rdm::cursorToString(ref).contains("canonicalizePath")) {
//             error() << Rdm::cursorToString(cursor) << "refs" << Rdm::cursorToString(clang_getCursorReferenced(cursor))
//                     << (clang_equalCursors(ref, clang_getCursorReferenced(cursor)) ? QByteArray() : ("changed to " + Rdm::cursorToString(ref)));
//         }
//     }
// #endif

    const CXCursorKind kind = clang_getCursorKind(cursor);
    switch (kind) {
    case CXCursor_CXXAccessSpecifier:
        return CXChildVisit_Recurse;
    default:
        break;
    }

    const RTags::Location loc = job->createLocation(cursor);
    if (loc.isNull()) {
        return CXChildVisit_Recurse;
    }
    CXCursor ref = clang_getCursorReferenced(cursor);
    RTags::Location refLoc;
    if (clang_equalCursors(cursor, ref) && !clang_isCursorDefinition(ref)) {
        // QByteArray old = Rdm::cursorToString(ref);
        ref = clang_getCursorDefinition(ref);
        // error() << "changed ref from" << old << "to" << Rdm::cursorToString(ref);
    }
    const CXCursorKind refKind = clang_getCursorKind(ref);

    if (kind == CXCursor_CallExpr && refKind == CXCursor_CXXMethod)
        return CXChildVisit_Recurse;

    if (clang_equalCursors(cursor, ref)) {
        refLoc.clear();
        if (!job->mIsPch) {
            CXString usr = clang_getCursorUSR(ref);
            const char *cstr = clang_getCString(usr);
            if (cstr) {
                refLoc = job->mPchUSRHash.value(QByteArray::fromRawData(cstr, strlen(cstr)));
            }
            clang_disposeString(usr);
        }
    } else {
        refLoc = job->createLocation(ref);
    }

    Rdm::CursorInfo &info = job->mSymbols[loc];
    if (!info.symbolLength) {
        if (job->mIsPch) {
            const QByteArray usr = Rdm::eatString(clang_getCursorUSR(cursor));
            if (!usr.isEmpty()) {
                job->mPchUSRHash[usr] = loc;
            }
        }
        info.kind = kind;
        CXString name;
        if (clang_isReference(kind)) {
            name = clang_getCursorSpelling(ref);
        } else {
            name = clang_getCursorSpelling(cursor);
        }
        const char *cstr = clang_getCString(name);
        info.symbolLength = cstr ? strlen(cstr) : 0;
        clang_disposeString(name);
#ifdef QT_DEBUG
        info.loc = loc;
        info.symbolName = Rdm::eatString(clang_getCursorDisplayName(cursor));
#endif
    } else if (info.kind == CXCursor_Constructor && kind == CXCursor_TypeRef) {
        return CXChildVisit_Recurse;
    }

    if (clang_isCursorDefinition(cursor) || kind == CXCursor_FunctionDecl) {
        job->addNamePermutations(cursor, loc);
    }

    if (!clang_isInvalid(refKind) && !refLoc.isNull()) {
        if (refLoc != loc) {
            info.target = refLoc;
        }
        Rdm::ReferenceType referenceType = Rdm::NormalReference;
        if (refKind == kind) {
            switch (refKind) {
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
        job->mReferences[loc] = qMakePair(refLoc, referenceType);
    } else if (kind == CXCursor_InclusionDirective) {
        CXFile includedFile = clang_getIncludedFile(cursor);
        CXString fileName = clang_getFileName(includedFile);
        const char* cstr = clang_getCString(fileName);
        if (cstr) {
            RTags::Location refLoc(Path::canonicalized(cstr), 0);
            info.target = refLoc;
            job->mReferences[loc] = qMakePair(refLoc, Rdm::NormalReference);
        }
        clang_disposeString(fileName);
    }
    return CXChildVisit_Recurse;

}


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

static QByteArray pchFileName(const QByteArray &path, const QByteArray &header)
{
    return path + SHA256::hash(header.constData());
}

void IndexerJob::run()
{
    QElapsedTimer timer;
    timer.start();
    QList<QByteArray> args = mArgs + mIndexer->mDefaultArgs;
    QList<Path> pchHeaders = extractPchFiles(args);
    if (!pchHeaders.isEmpty()) {
        QMutexLocker locker(&mIndexer->mMutex);
        bool wait;
        do {
            wait = false;
            foreach (const QByteArray &pchHeader, pchHeaders) {
                if (mIndexer->mPchHeaderError.contains(pchHeader)) {
                    int idx = args.indexOf(pchHeader);
                    Q_ASSERT(idx > 0);
                    args.removeAt(idx);
                    args.removeAt(idx - 1);
                } else if (mIndexer->mIndexing.contains(pchHeader)) {
                    wait = true;
                    break;
                }
            }
            if (wait) {
                mIndexer->mCondition.wait(&mIndexer->mMutex);
                if (mAborted)
                    return;
            }
        } while (wait);
        mPchUSRHash = mIndexer->pchUSRHash(pchHeaders);
    }
    const quint64 waitingForPch = timer.restart();

    QVarLengthArray<const char*, 32> clangArgs(args.size());
    QByteArray clangLine = "clang ";
    bool nextIsPch = false, nextIsX = false;
    QByteArray pchName;

    QList<Path> pchFiles;
    int idx = 0;
    foreach (const QByteArray& arg, args) {
        if (arg.isEmpty())
            continue;

        if (nextIsPch) {
            nextIsPch = false;
            pchFiles.append(pchFileName(mIndexer->mPath, arg));
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
        pchName = pchFileName(mIndexer->mPath, mIn);
    }
    clangLine += mIn;

    CXIndex index = clang_createIndex(1, 1);
    CXTranslationUnit unit = clang_parseTranslationUnit(index, mIn.constData(),
                                                        clangArgs.data(), idx, 0, 0,
                                                        CXTranslationUnit_Incomplete | CXTranslationUnit_DetailedPreprocessingRecord);
    const time_t timeStamp = time(0);
    warning() << "loading unit" << clangLine << (unit != 0);
    bool pchError = false;

    if (!unit) {
        pchError = mIsPch;
        error() << "got 0 unit for" << clangLine;
        mDependencies[mIn].insert(mIn);
        QCoreApplication::postEvent(mIndexer, new DependencyEvent(mDependencies));
        mIndexer->mSyncer->addFileInformation(mIn, mArgs, timeStamp);
    } else {
        clang_getInclusions(unit, inclusionVisitor, this);
        foreach(const Path &pchHeader, pchHeaders) {
            foreach(const Path &dep, mIndexer->pchDependencies(pchHeader)) {
                mDependencies[dep].insert(mIn);
            }
        }
        QCoreApplication::postEvent(mIndexer, new DependencyEvent(mDependencies));

        clang_visitChildren(clang_getTranslationUnitCursor(unit), indexVisitor, this);
        if (mIsPch) {
            Q_ASSERT(!pchName.isEmpty());
            if (clang_saveTranslationUnit(unit, pchName.constData(), clang_defaultSaveOptions(unit)) != CXSaveError_None) {
                error() << "Couldn't save pch file" << mIn << pchName;
                pchError = true;
            } else {
                mIndexer->setPchUSRHash(mIn, mPchUSRHash);
            }
        }
        clang_disposeTranslationUnit(unit);

        foreach (const Path &path, mPaths) {
            const RTags::Location loc(path, 0);
            mSymbolNames[path].insert(loc);
            mSymbolNames[path.fileName()].insert(loc);
        }
        if (!mAborted) {
            mIndexer->mSyncer->addSymbols(mSymbols);
            mIndexer->mSyncer->addSymbolNames(mSymbolNames);
            mIndexer->mSyncer->addFileInformation(mIn, mArgs, timeStamp);
            mIndexer->mSyncer->addReferences(mReferences);
            if (mIsPch)
                mIndexer->setPchDependencies(mIn, mPchDependencies);
        }

    }
    clang_disposeIndex(index);
    if (mIsPch && !mAborted) {
        QMutexLocker locker(&mIndexer->mMutex);
        if (pchError) {
            mIndexer->mPchHeaderError.insert(mIn);
        } else {
            mIndexer->mPchHeaderError.remove(mIn);
        }
    }

    error() << "visited" << mIn << timer.elapsed()
            << qPrintable(waitingForPch ? QString("Waited for pch: %1ms.").arg(waitingForPch) : QString());
    finish(mIn);
}
