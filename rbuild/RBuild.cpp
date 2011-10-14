#include "RBuild.h"
#include <QCoreApplication>
#include <QtAlgorithms>
#include <clang-c/Index.h>
#include <stdio.h>

RBuild::Entry::Entry()
{
}

RBuild::Entry::~Entry()
{
    if (parent) {
        QList<Entry*>::iterator it = qFind(parent->children.begin(), parent->children.end(), this);
        Q_ASSERT(it != parent->children.end());
        parent->children.erase(it);
    }
    qDeleteAll(children);
}

RBuild::RBuild(QObject *parent)
    : QObject(parent)
{
}

void RBuild::init(const Path& makefile)
{
    connect(&mParser, SIGNAL(fileReady(const MakefileItem&)),
            this, SLOT(makefileFileReady(const MakefileItem&)));
    connect(&mParser, SIGNAL(done()), this, SLOT(makefileDone()));
    mParser.run(makefile);
}

static inline void printTree(const QList<RBuild::Entry*>& tree)
{
    static int indent = 0;
    foreach(const RBuild::Entry* entry, tree) {
        for (int i = 0; i < (indent * 4); ++i)
            printf(" ");
        CXString kindstr = clang_getCursorKindSpelling(static_cast<CXCursorKind>(entry->cxKind));
        printf("%s at %s:%u:%u with kind %s (%p)\n",
               entry->field.constData(),
               entry->file.constData(),
               entry->line, entry->column, clang_getCString(kindstr),
               entry);
        clang_disposeString(kindstr);
        ++indent;
        //printTree(entry->children);
        --indent;
    }
}

void RBuild::makefileDone()
{
    printf("printTree\n");
    printTree(mEntries);
    fprintf(stderr, "Done.\n");

    qDeleteAll(mEntries);
    qApp->quit();
}

void RBuild::makefileFileReady(const MakefileItem& file)
{
    compile(file.arguments);
}

static inline QByteArray cursorKey(const CXCursor& cursor)
{
    CXSourceLocation loc = clang_getCursorLocation(cursor);
    CXString usr = clang_getCursorUSR(cursor);
    CXFile file;
    unsigned int line, col, off;
    clang_getInstantiationLocation(loc, &file, &line, &col, &off);
    CXString filename = clang_getFileName(file);

    /*CXCursorKind kind = clang_getCursorKind(cursor);
    QByteArray data(clang_getCString(filename));
    data += QByteArray::number(line) + "-" + QByteArray::number(col) + "-" + QByteArray::number(off) + "-" + QByteArray::number(kind);
    clang_disposeString(filename);
    return data;*/

    QByteArray key;
    const char* str = clang_getCString(usr);
    if (str && (strcmp(str, "") != 0))
        key += QByteArray(str) + "-";
    else {
        CXString spelling = clang_getCursorSpelling(cursor);
        str = clang_getCString(spelling);
        if (str && (strcmp(str, "") != 0))
            key += QByteArray(str) + "-";
        clang_disposeString(spelling);
    }
    key += QByteArray(clang_getCString(filename)) + "-";
    key += QByteArray::number(line) + "-" + QByteArray::number(col) + "-" + QByteArray::number(off);

    clang_disposeString(filename);
    clang_disposeString(usr);

    return key;
}

static inline bool equalCursor(const CXCursor& c1, const CXCursor& c2)
{
    return (cursorKey(c1) == cursorKey(c2));
}

struct CollectData
{
    CXCursor unitCursor;
    QSet<QByteArray> seen;
    QList<CXCursor> cursors;
};

static inline void addCursor(const CXCursor& cursor, CollectData* data)
{
    const QByteArray key = cursorKey(cursor);
    if (!data->seen.contains(key))
        data->cursors.append(cursor);
}

static CXChildVisitResult collectSymbols(CXCursor cursor, CXCursor, CXClientData client_data)
{
    CollectData* data = reinterpret_cast<CollectData*>(client_data);
    addCursor(cursor, data);
    addCursor(clang_getCursorDefinition(cursor), data);
    addCursor(clang_getCursorReferenced(cursor), data);
    const unsigned int decl = clang_getNumOverloadedDecls(cursor);
    for (unsigned int i = 0; i < decl; ++i)
        addCursor(clang_getOverloadedDecl(cursor, i), data);
    return CXChildVisit_Recurse;
}

static inline void debugCursor(FILE* out, const CXCursor& cursor)
{
    CXFile file;
    unsigned int line, col, off;
    CXSourceLocation loc = clang_getCursorLocation(cursor);
    clang_getInstantiationLocation(loc, &file, &line, &col, &off);
    CXString name = clang_getCursorSpelling(cursor);
    CXString filename = clang_getFileName(file);
    CXString kind = clang_getCursorKindSpelling(clang_getCursorKind(cursor));
    fprintf(out, "cursor name %s, kind %s, loc %s:%u:%u\n",
            clang_getCString(name), clang_getCString(kind), clang_getCString(filename), line, col);
    clang_disposeString(name);
    clang_disposeString(kind);
    clang_disposeString(filename);
}

#ifdef QT_DEBUG
static inline void verifyTree(const QList<RBuild::Entry*> toplevels, const QHash<QByteArray, RBuild::Entry*>& seen)
{
    printf("verify!\n");
    const RBuild::Entry* top;
    foreach(const RBuild::Entry* entry, seen) {
        // verify parents
        top = entry;
        while (top->parent) {
            if (top == top->parent) {
                fprintf(stderr, "parent is itself, %s (%s:%u:%u)\n",
                        top->field.constData(),
                        top->file.constData(),
                        top->line, top->column);
            }
            Q_ASSERT(top != top->parent);
            top = top->parent;
        }
        Q_ASSERT(top != 0);
        QList<RBuild::Entry*>::const_iterator it = qFind(toplevels, top);
        if (it == toplevels.end()) {
            fprintf(stderr, "verify failure, %s (%s:%u:%u) has %s (%s:%u:%u) as parent\n",
                    entry->field.constData(),
                    entry->file.constData(),
                    entry->line, entry->column,
                    top->field.constData(),
                    top->file.constData(),
                    top->line, top->column);
        }
        Q_ASSERT(it != toplevels.end());
    }
}
#endif

static inline CXCursor parentForCursor(const CXCursor& cursor)
{
    if (!clang_isCursorDefinition(cursor)) {
        /*
        CXCursor def = clang_getCursorDefinition(cursor);
        if (isValidCursor(def) && !equalCursor(def, cursor))
            return def;
            */

        //CXCursor def = clang_getCursorDefinition(cursor);
        CXCursor ref = clang_getCursorReferenced(cursor);
        CXCursor can = clang_getCanonicalCursor(cursor);
        CXCursorKind cxkind = clang_getCursorKind(cursor);
        //const bool hasDef = isValidCursor(def) && !equalCursor(cursor, def);
        const bool hasRef = isValidCursor(ref) && !equalCursor(cursor, ref);
        const bool hasCan = isValidCursor(can) && !equalCursor(cursor, can)
                            && (cxkind != CXCursor_ClassDecl && cxkind != CXCursor_StructDecl)
                            && clang_isCursorDefinition(can);
        const bool hasOverloads = (clang_getNumOverloadedDecls(cursor) > 0) && false;

#if 0
        QByteArray name = eatString(clang_getCursorSpelling(cursor));
        CXSourceLocation loc;
        CXFile file;
        unsigned int line, col, off;
        loc = clang_getCursorLocation(cursor);
        clang_getInstantiationLocation(loc, &file, &line, &col, &off);
        QByteArray filename = eatString(clang_getFileName(file));

        if (name == "compile" && (filename.contains("RBuild"))) {
            fprintf(stderr, "Putting cursor %d %d %d\n", hasRef, hasCan, hasOverloads);
            debugCursor(stderr, cursor);
        }
#endif

        if (hasRef || hasOverloads || hasCan) {
            CXCursor parent;
            if (hasCan) {
                parent = can;
            } else if (hasRef) {
                parent = ref;
            } else {
                Q_ASSERT(hasOverloads);
                parent = clang_getOverloadedDecl(cursor, 0);
            }
            /*
            if (!clang_isCursorDefinition(parent)) {
                CXCursor def = clang_getCursorDefinition(parent);
                if (isValidCursor(def)) {
                    parent = def;
                } else if (hasCan) {
                    return clang_getNullCursor();
                }
            }
            */
            // we have a valid definition, put this one under that one
            return parent;
        }
    }
    return clang_getNullCursor();
}

static inline RBuild::Entry *findContainer(CXCursor cursor, const QHash<QByteArray, RBuild::Entry*> &seen)
{
    do {
        cursor = clang_getCursorSemanticParent(cursor);
        switch (clang_getCursorKind(cursor)) {
        case CXCursor_StructDecl:
        case CXCursor_ClassDecl:
        case CXCursor_Namespace:
            Q_ASSERT(seen.contains(cursorKey(cursor)));
            return seen[cursorKey(cursor)];
        default:
            break;
        }
    } while (isValidCursor(cursor));
    // ### add functions as possible containers
    return 0;
}

static inline void resolveData(const CollectData& data, QList<RBuild::Entry*>& entries, QHash<QByteArray, RBuild::Entry*>& seen)
{
    CXString cxkindstr, cxfilestr, cxnamestr;
    CXCursorKind cxkind;
    CXSourceLocation loc;
    CXFile file;
    unsigned int line, col, off;

    QList<QPair<CXCursor, RBuild::Entry*> > tryLater;

    foreach(const CXCursor& cursor, data.cursors) {
        QByteArray key = cursorKey(cursor);
        if (seen.contains(key))
            continue;

        loc = clang_getCursorLocation(cursor);
        clang_getInstantiationLocation(loc, &file, &line, &col, &off);

        cxkind = clang_getCursorKind(cursor);
        cxkindstr = clang_getCursorKindSpelling(cxkind);
        cxnamestr = clang_getCursorSpelling(cursor);
        cxfilestr = clang_getFileName(file);

        QByteArray name = QByteArray(clang_getCString(cxnamestr));
        QByteArray filename = QByteArray(clang_getCString(cxfilestr));

        if (!key.isEmpty() && !name.isEmpty() && !filename.isEmpty()) {
            CXCursor parent = parentForCursor(cursor);
            if (isValidCursor(parent)) {
#if 1
                QByteArray name = eatString(clang_getCursorSpelling(cursor));
                CXSourceLocation loc;
                CXFile file;
                unsigned int line, col, off;
                loc = clang_getCursorLocation(cursor);
                clang_getInstantiationLocation(loc, &file, &line, &col, &off);
                QByteArray filename = eatString(clang_getFileName(file));

                if (name == "compile" && (filename.contains("RBuild"))) {
                    fprintf(stderr, "Putting cursor\n");
                    debugCursor(stderr, cursor);
                    debugCursor(stderr, parent);
                }
#endif

                RBuild::Entry* entry = new RBuild::Entry;
                entry->container = findContainer(cursor, seen);
                entry->parent = 0;
                entry->field = name;
                entry->file = filename;
                entry->line = line;
                entry->column = col;
                entry->cxKind = cxkind;
                seen[key] = entry;

                QByteArray parentKey = cursorKey(parent);
                QHash<QByteArray, RBuild::Entry*>::iterator it = seen.find(parentKey);
                if (it != seen.end()) {
                    it.value()->children.append(entry);
                    entry->parent = it.value();
                } else { // punt
                    tryLater.append(qMakePair(cursor, entry));
                }
            } else { // assume that we are a definition?
                if ((cxkind >= CXCursor_FirstDecl && cxkind <= CXCursor_LastDecl)
                    || cxkind == CXCursor_CXXBaseSpecifier
                    || cxkind == CXCursor_LabelStmt) {
                    RBuild::Entry* entry = new RBuild::Entry;
                    entry->container = findContainer(cursor, seen);
                    entry->parent = 0;
                    entry->field = name;
                    entry->file = filename;
                    entry->line = line;
                    entry->column = col;
                    entry->cxKind = cxkind;
                    seen[key] = entry;

                    entries.append(entry);
                } else {
                    //fprintf(stderr, "No definition %s %s at %s:%u:%u\n", name.constData(), clang_getCString(cxkindstr),
                    //        filename.constData(), line, col);
                    if (cxkind != CXCursor_OverloadedDeclRef && cxkind != CXCursor_TypeRef) {
                        Q_ASSERT(false && "No definition!");
                    }
                }
            }
        }

        clang_disposeString(cxnamestr);
        clang_disposeString(cxfilestr);
        clang_disposeString(cxkindstr);
    }

    QList<QPair<CXCursor, RBuild::Entry*> >::iterator it = tryLater.begin();
    while (it != tryLater.end()) {
        CXCursor parent = parentForCursor((*it).first);
        if (isValidCursor(parent)) {
            QByteArray key = cursorKey(parent);
            QHash<QByteArray, RBuild::Entry*>::iterator it2 = seen.find(key);
            if (it2 != seen.end()) {
                // append as child
                it2.value()->children.append((*it).second);
                (*it).second->parent = it2.value();
                it = tryLater.erase(it);
                continue;
            }
        }
        ++it;
    }

#ifdef QT_DEBUG
    it = tryLater.begin();
    while (it != tryLater.end()) {
        printf("remaining %s\n", (*it).second->field.constData());
        debugCursor(stdout, (*it).first);
        printf("----\n\n");
        ++it;
    }

    verifyTree(entries, seen);
#endif

    Q_ASSERT(tryLater.isEmpty());
}

void RBuild::compile(const GccArguments& arguments)
{
    CXIndex idx = clang_createIndex(0, 1);
    foreach(const Path& input, arguments.input()) {
        if (!input.endsWith("/RBuild.cpp")
            && !input.endsWith("/main.cpp")) {
            printf("skipping %s\n", input.constData());
            continue;
        }
        fprintf(stderr, "parsing %s\n", input.constData());

        QList<QByteArray> arglist;
        arglist += arguments.arguments("-I");
        arglist += arguments.arguments("-D");
        // ### not very efficient
        QVector<const char*> argvector;
        foreach(const QByteArray& arg, arglist) {
            argvector.append(arg.constData());
        }

        CXTranslationUnit unit = clang_parseTranslationUnit(idx, input.constData(), argvector.constData(), argvector.size(), 0, 0, 0);
        if (!unit) {
            fprintf(stderr, "Unable to parse unit for %s\n", input.constData());
            continue;
        }

        const unsigned int numDiags = clang_getNumDiagnostics(unit);
        for (unsigned int i = 0; i < numDiags; ++i) {
            CXDiagnostic diag = clang_getDiagnostic(unit, i);
            CXString diagString = clang_formatDiagnostic(diag,
                                                         CXDiagnostic_DisplaySourceLocation |
                                                         CXDiagnostic_DisplayColumn |
                                                         CXDiagnostic_DisplayCategoryName);
            fprintf(stderr, "%s\n", clang_getCString(diagString));
            clang_disposeString(diagString);
            clang_disposeDiagnostic(diag);
        }

        CollectData data;
        data.unitCursor = clang_getTranslationUnitCursor(unit);
        clang_visitChildren(data.unitCursor, collectSymbols, &data);

        resolveData(data, mEntries, mSeen);

        clang_disposeTranslationUnit(unit);
    }
    clang_disposeIndex(idx);
}
