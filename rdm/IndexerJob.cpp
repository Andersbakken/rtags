#include "IndexerJob.h"
#include <Path.h>
#include <clang-c/Index.h>
#include <QMutexLocker>

static inline void addInclusion(IndexerJob* job, CXFile inc)
{
    CXString str = clang_getFileName(inc);

    const QByteArray path = Path::resolved(clang_getCString(str));

    QMutexLocker locker(&job->m_impl->incMutex);
    if (!qstrcmp(job->m_in, path)) {
        clang_disposeString(str);
        return;
    }

    job->m_impl->incs[path].insert(job->m_in);
    clang_disposeString(str);
}

static void inclusionVisitor(CXFile included_file,
                             CXSourceLocation*,
                             unsigned include_len,
                             CXClientData client_data)
{
    IndexerJob* job = static_cast<IndexerJob*>(client_data);
    if (include_len)
        addInclusion(job, included_file);
}

static inline void addNamePermutations(CXCursor cursor, const char* usr, IndexerJob* job)
{
    QByteArray qusr = QByteArray(usr), qname;
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
        job->m_syms[qparam].insert(qusr);
        if (qparam != qnoparam)
            job->m_syms[qnoparam].insert(qusr);

        clang_disposeString(displayName);
        cur = clang_getCursorSemanticParent(cur);
    }
}

static CXChildVisitResult indexVisitor(CXCursor cursor,
                                       CXCursor /*parent*/,
                                       CXClientData client_data)
{
    IndexerJob* job = static_cast<IndexerJob*>(client_data);

    CXCursorKind kind = clang_getCursorKind(cursor);
    switch (kind) {
    case CXCursor_CXXAccessSpecifier:
        return CXChildVisit_Recurse;
    default:
        break;
    }

    CXString usr = clang_getCursorUSR(cursor);
    const char* cusr = clang_getCString(usr);
    int usrlen = strlen(cusr);
    if (!usrlen || (usrlen == 2 && !strncmp(cusr, "c:", 2))) {
        clang_disposeString(usr);
        CXCursor ref = clang_getCursorReferenced(cursor);
        usr = clang_getCursorUSR(ref);
        cusr = clang_getCString(usr);
        usrlen = strlen(cusr);
        if (!usrlen || (usrlen == 2 && !strncmp(cusr, "c:", 2))) {
            clang_disposeString(usr);
            return CXChildVisit_Recurse;
        }
    }
    //CXString kindstr = clang_getCursorKindSpelling(kind);

    CXSourceLocation loc = clang_getCursorLocation(cursor);
    CXFile file;
    unsigned int line, col;
    clang_getSpellingLocation(loc, &file, &line, &col, 0);
    CXString fileName = clang_getFileName(file);
    const char* cfileName = clang_getCString(fileName);
    if (!cfileName || !strlen(cfileName)) {
        //clang_disposeString(kindstr);
        clang_disposeString(usr);
        clang_disposeString(fileName);
        return CXChildVisit_Recurse;
    }
    QByteArray qloc(Path::resolved(cfileName));
    qloc += ":" + QByteArray::number(line) + ":" + QByteArray::number(col);

    if (clang_isCursorDefinition(cursor)
        || kind == CXCursor_FunctionDecl) {
        job->m_defs[cusr].insert(qloc);
        addNamePermutations(cursor, cusr, job);
    }
    job->m_refs[cusr].insert(qloc);

    Q_ASSERT(strcmp(cusr, "") != 0);
    Q_ASSERT(strcmp(cusr, "c:") != 0);
    //clang_disposeString(kindstr);
    clang_disposeString(fileName);
    clang_disposeString(usr);

    return CXChildVisit_Recurse;
}

IndexerJob::IndexerJob(IndexerImpl* impl, Indexer::Type type, Indexer::Mode mode, int id,
                       const QByteArray& path, const QByteArray& input, const QByteArray& output,
                       const QList<QByteArray>& arguments)
    : m_type(type), m_mode(mode), m_id(id), m_path(path), m_in(input), m_out(output), m_args(arguments), m_impl(impl)
{
}

inline void IndexerJob::addFileNameSymbol(const QByteArray& fileName)
{
    // ### would it be faster/better to use QFileInfo here?
    int idx = -1;
    for (;;) {
        int backslashes = 0;
        idx = fileName.lastIndexOf('/', idx);
        while (idx > 0 && fileName.at(idx - 1) == '\\') {
            ++backslashes;
            --idx;
        }
        if ((backslashes % 2) || !idx) {
            idx -= 1;
            if (!idx)
                break;
        } else {
            idx += backslashes;
            break;
        }
    }
    if (idx == -1)
        return;
    m_syms[fileName.mid(idx + 1)].insert(fileName + ":1:1");
}

static inline void uniteSets(HashSet& dst, HashSet& src)
{
    HashSet::const_iterator it = src.begin();
    const HashSet::const_iterator end = src.end();
    while (it != end) {
        dst[it.key()].unite(it.value());
        ++it;
    }
    src.clear();
}

static inline QList<QByteArray> extractPchFiles(const QList<QByteArray>& args)
{
    QList<QByteArray> out;
    bool nextIsPch = false;
    foreach(const QByteArray& arg, args) {
        if (arg.isEmpty())
            continue;

        if (nextIsPch) {
            nextIsPch = false;
            out.append(arg);
        } else if (arg == "-include-pch")
            nextIsPch = true;
    }
    return out;
}

void IndexerJob::run()
{
    int unitMode = UnitCache::Source | UnitCache::AST;
    if (m_mode != Indexer::Force)
        unitMode |= UnitCache::Memory;

    QList<QByteArray> pchFiles = extractPchFiles(m_args);
    if (!pchFiles.isEmpty()) {
        QMutexLocker locker(&m_impl->implMutex);
        bool wait;
        do {
            wait = false;
            foreach(const QByteArray& pchFile, pchFiles) {
                if (m_impl->indexing.contains(pchFile)) {
                    wait = true;
                    break;
                }
            }
            if (wait)
                m_impl->implCond.wait(&m_impl->implMutex);
        } while (wait);
    }

    // ### hack for now
    CachedUnit unit(m_in, m_in, m_args, unitMode);

    if (unit.unit()) {
        qDebug() << "parsing" << m_in << unit.unit()->fileName;
        CXTranslationUnit tu = unit.unit()->unit;
        unsigned int diagCount = clang_getNumDiagnostics(tu);
        for (unsigned int i = 0; i < diagCount; ++i) {
            const CXDiagnostic diag = clang_getDiagnostic(tu, i);
            const CXDiagnosticSeverity severity = clang_getDiagnosticSeverity(diag);
            if (severity >= CXDiagnostic_Warning) {
                CXString msg = clang_formatDiagnostic(diag, CXDiagnostic_DisplaySourceLocation
                                                      | CXDiagnostic_DisplayColumn
                                                      | CXDiagnostic_DisplayOption
                                                      | CXDiagnostic_DisplayCategoryName);
                qWarning("clang: %s", clang_getCString(msg));
                clang_disposeString(msg);
            }
            clang_disposeDiagnostic(diag);
        }

        if (unit.unit()->origin == UnitCache::Source) {
            qDebug() << "reread" << unit.unit()->fileName << "from source, revisiting";
            clang_getInclusions(tu, inclusionVisitor, this);
            clang_visitChildren(clang_getTranslationUnitCursor(tu), indexVisitor, this);
            addFileNameSymbol(unit.unit()->fileName);

            QMutexLocker deflocker(&m_impl->defMutex);
            uniteSets(m_impl->defs, m_defs);
            deflocker.unlock();
            QMutexLocker reflocker(&m_impl->refMutex);
            uniteSets(m_impl->refs, m_refs);
            reflocker.unlock();
            QMutexLocker symlocker(&m_impl->symMutex);
            uniteSets(m_impl->syms, m_syms);
            symlocker.unlock();
        }
    } else {
        qDebug() << "got 0 unit for" << m_in;
    }

    emit done(m_id, m_in, m_out);
}
