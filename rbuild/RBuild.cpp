#include "RBuild.h"
#include <QCoreApplication>
#include <QtAlgorithms>
#include <clang-c/Index.h>
#include <stdio.h>

class CursorKey
{
public:
    CursorKey()
        : kind(CXCursor_FirstInvalid), file(0)
    {}
    CursorKey(const CXCursor &cursor)
        : kind(clang_getCursorKind(cursor)), file(0)
    {
        if (!clang_isInvalid(kind)) {
            CXSourceLocation loc = clang_getCursorLocation(cursor);
            clang_getInstantiationLocation(loc, &file, &line, &col, &off);
            if (file)
                fileName = clang_getFileName(file);
        }
    }

    ~CursorKey()
    {
        if (file)
            clang_disposeString(fileName);
    }

    bool isNull() const
    {
        return !file;
    }
    bool operator<(const CursorKey &other) const
    {
        if (isNull())
            return true;
        if (other.isNull())
            return false;
        const int ret = strcmp(clang_getCString(fileName), clang_getCString(other.fileName));
        if (ret < 0)
            return true;
        if (ret > 0)
            return false;
        return kind < other.kind;
    }

    bool operator==(const CursorKey &other) const
    {
        if (isNull())
            return other.isNull();
        return kind == other.kind && !strcmp(clang_getCString(fileName), clang_getCString(other.fileName));
    }

    CXCursorKind kind;
    CXFile file;
    CXString fileName;
    unsigned line, col, off;
};

static inline uint qHash(const CursorKey &key)
{
    uint h = 0;
    if (!key.isNull()) {
#define HASHCHAR(ch)                            \
        h = (h << 4) + ch;                      \
        h ^= (h & 0xf0000000) >> 23;            \
        h &= 0x0fffffff;                        \
        ++h;

        const char *ch = clang_getCString(key.fileName);
        Q_ASSERT(ch);
        while (*ch) {
            HASHCHAR(*ch);
            ++ch;
        }
        const uint16_t uints[] = { key.kind, key.line, key.col };
        for (int i=0; i<3; ++i) {
            ch = reinterpret_cast<const char*>(&uints[i]);
            for (int j=0; j<2; ++j) {
                HASHCHAR(*ch);
                ++ch;
            }
        }
    }
    return h;
}

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
        printTree(entry->children);
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
        CXString spelling = clang_getCursorDisplayName(cursor);
        str = clang_getCString(spelling);
        if (str && (strcmp(str, "") != 0))
            key += QByteArray(str) + "-";
        clang_disposeString(spelling);
    }
    key += QByteArray(clang_getCString(filename)) + "-";
    key += QByteArray::number(line) + ":" + QByteArray::number(col) + ":" + QByteArray::number(off);

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

static inline void addCursor_helper(const CXCursor& cursor, CollectData* data)
{
    const QByteArray key = cursorKey(cursor);
    if (!data->seen.contains(key)) {
        data->cursors.append(cursor);
        data->seen.insert(key);
    }
}

static inline void addCursor(const CXCursor& cursor, CollectData* data)
{
    CXCursor parent = cursor;
    do {
        addCursor_helper(parent, data);
        parent = clang_getCursorSemanticParent(parent);
    } while (isValidCursor(parent));
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
    CXString name = clang_getCursorDisplayName(cursor);
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

static inline void replaceEntry(RBuild::Entry* entry, const CXCursor& cursor)
{
    CXSourceLocation loc;
    CXFile file;
    unsigned int line, col, off;

    loc = clang_getCursorLocation(cursor);
    clang_getInstantiationLocation(loc, &file, &line, &col, &off);

    entry->container = 0;
    entry->parent = 0;
    entry->field = eatString(clang_getCursorDisplayName(cursor));
    entry->file = eatString(clang_getFileName(file));
    entry->line = line;
    entry->column = col;
    entry->cxKind = clang_getCursorKind(cursor);
}

static inline RBuild::Entry* createEntry(const CXCursor& cursor)
{
    RBuild::Entry* entry = new RBuild::Entry;
    replaceEntry(entry, cursor);
    return entry;
}

static inline RBuild::Entry *findContainer(CXCursor cursor, QHash<QByteArray, RBuild::Entry*> &seen, QList<RBuild::Entry*> &entries);

static inline RBuild::Entry* createEntry(const CXCursor& cursor, QHash<QByteArray, RBuild::Entry*> &seen, QList<RBuild::Entry*> &entries)
{
    CXSourceLocation loc;
    CXFile file;
    unsigned int line, col, off;

    QByteArray key = cursorKey(cursor);
    if (seen.contains(key))
        return 0;

    loc = clang_getCursorLocation(cursor);
    clang_getInstantiationLocation(loc, &file, &line, &col, &off);
    QByteArray filename = eatString(clang_getFileName(file));

    CXCursor canonical = clang_getCanonicalCursor(cursor);
    if (isValidCursor(canonical)) {
        RBuild::Entry* parentEntry = 0;
        CXCursor definition = clang_isCursorDefinition(cursor) ? clang_getNullCursor() : clang_getCursorDefinition(canonical);
        if (isValidCursor(definition) && !equalCursor(canonical, definition)) {
            QByteArray definitionKey = cursorKey(definition);
            if (!seen.contains(definitionKey)) {
                QByteArray canonicalKey = cursorKey(canonical);
                if (seen.contains(canonicalKey)) {
                    parentEntry = seen.value(canonicalKey);
                    replaceEntry(parentEntry, definition);
                    seen[definitionKey] = parentEntry;
                    parentEntry->container = findContainer(definition, seen, entries);
                } else {
                    parentEntry = createEntry(definition);
                    seen[definitionKey] = parentEntry;
                    entries.append(parentEntry);
                    parentEntry->container = findContainer(definition, seen, entries);
                }
            } else
                parentEntry = seen.value(definitionKey);
        } else if (!equalCursor(cursor, canonical)) {
            QByteArray canonicalKey = cursorKey(canonical);
            if (!seen.contains(canonicalKey)) {
                parentEntry = createEntry(canonical);
                seen[canonicalKey] = parentEntry;
                entries.append(parentEntry);
                parentEntry->container = findContainer(canonical, seen, entries);
            } else
                parentEntry = seen.value(canonicalKey);
        }
        RBuild::Entry* entry = createEntry(cursor);
        if (parentEntry) {
            parentEntry->children.append(entry);
            entry->parent = parentEntry;
        } else {
            entries.append(entry);
        }
        seen[key] = entry;
        entry->container = findContainer(cursor, seen, entries);
        return entry;
    } else {
        if (!filename.isEmpty()) {
            debugCursor(stderr, cursor);
            qFatal("non-canonical");
        }
    }
    return 0;
}

static inline RBuild::Entry *findContainer(CXCursor cursor, QHash<QByteArray, RBuild::Entry*> &seen, QList<RBuild::Entry*> &entries)
{
    CXCursor parent = cursor;
    do {
        parent = clang_getCursorSemanticParent(parent);
        switch (clang_getCursorKind(parent)) {
        case CXCursor_StructDecl:
        case CXCursor_ClassDecl:
        case CXCursor_Namespace: {
            if (eatString(clang_getCursorDisplayName(parent)).isEmpty())
                break;
            const QByteArray containerKey = cursorKey(parent);
            if (!seen.contains(containerKey)) {
                RBuild::Entry* entry = createEntry(parent, seen, entries);
                Q_ASSERT(entry && seen.value(containerKey) == entry);
                return entry;
            }
            return seen[containerKey]; }
        default:
            break;
        }
    } while (isValidCursor(parent));
    // ### add functions as possible containers
    return 0;
}

static inline void resolveData(const CollectData& data, QList<RBuild::Entry*>& entries, QHash<QByteArray, RBuild::Entry*>& seen)
{
    foreach(const CXCursor& cursor, data.cursors) {
        (void)createEntry(cursor, seen, entries);
    }
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
