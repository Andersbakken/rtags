#include "RBuild.h"
#include <QCoreApplication>
#include <QtAlgorithms>
#include <sstream>
#include <clang-c/Index.h>
#include <leveldb/db.h>
#include <stdio.h>
#include <stdint.h>

class CursorKey
{
public:
    CursorKey()
        : kind(CXCursor_FirstInvalid)
    {}
    CursorKey(const CXCursor &cursor)
        : kind(clang_getCursorKind(cursor))
    {
        if (!clang_isInvalid(kind)) {
            CXSourceLocation loc = clang_getCursorLocation(cursor);
            CXFile file;
            clang_getInstantiationLocation(loc, &file, &line, &col, &off);
            fileName = Path::resolved(eatString(clang_getFileName(file)));
            symbolName = eatString(clang_getCursorDisplayName(cursor));
        }
    }

    bool isValid() const
    {
        return !fileName.isEmpty() && !symbolName.isEmpty();
    }

    bool isNull() const
    {
        return fileName.isEmpty();
    }

    bool operator<(const CursorKey &other) const
    {
        if (!isValid())
            return true;
        if (!other.isValid())
            return false;
        int ret = qstrcmp(fileName, other.fileName);
        if (ret < 0)
            return true;
        if (ret > 0)
            return false;
        if (off < other.off)
            return true;
        if (off > other.off)
            return false;
        ret = qstrcmp(symbolName, other.symbolName);
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
        return (kind == other.kind
                && off == other.off
                && fileName == other.fileName
                && symbolName == other.symbolName);
    }

    CXCursorKind kind;
    QByteArray fileName;
    QByteArray symbolName;
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

        const char *ch = key.fileName.constData();
        Q_ASSERT(ch);
        while (*ch) {
            HASHCHAR(*ch);
            ++ch;
        }
        ch = key.symbolName.constData();
        Q_ASSERT(ch);
        while (*ch) {
            HASHCHAR(*ch);
            ++ch;
        }
        const uint16_t uints[] = { key.kind, key.off };
        for (int i=0; i<2; ++i) {
            ch = reinterpret_cast<const char*>(&uints[i]);
            for (int j=0; j<2; ++j) {
                HASHCHAR(*ch);
                ++ch;
            }
        }
    }
    return h;
}

struct CollectData
{
    struct Data
    {
        CursorKey cursor;
        QList<CursorKey> parents;
    };
    struct DataEntry
    {
        Data cursor;
        Data reference;
        Data canonical;
    };

    QSet<CursorKey> seen;
    QList<DataEntry> data;
};

RBuild::RBuild(QObject *parent)
    : QObject(parent), mData(0)
{
}

RBuild::~RBuild()
{
    delete mData;
}

void RBuild::init(const Path& makefile)
{
    connect(&mSysInfo, SIGNAL(done()), this, SLOT(startParse()));
    mSysInfo.init();
    mMakefile = makefile;
}

void RBuild::startParse()
{
    connect(&mParser, SIGNAL(fileReady(const MakefileItem&)),
            this, SLOT(makefileFileReady(const MakefileItem&)));
    connect(&mParser, SIGNAL(done()), this, SLOT(makefileDone()));
    mParser.run(mMakefile);
}

void RBuild::makefileDone()
{
    fprintf(stderr, "Done parsing, now writing.\n");
    writeData(".rtags.db");
    fprintf(stderr, "All done.\n");

    qApp->quit();
}

void RBuild::makefileFileReady(const MakefileItem& file)
{
    compile(file.arguments);
}

static inline std::string cursorKeyToString(const CursorKey& key)
{
    std::stringstream stream;
    stream << key.fileName.constData();
    stream << ":" << key.line << ":" << key.col;
    return stream.str();
}

static inline void writeEntry(leveldb::DB* db, const leveldb::WriteOptions& opt, const CollectData::DataEntry& entry)
{
    const CursorKey& key = entry.cursor.cursor;
    const CursorKey& val = entry.reference.cursor;
    if (!key.isValid() || !val.isValid())
        return;
    db->Put(opt, cursorKeyToString(key), cursorKeyToString(val));
}

void RBuild::writeData(const QByteArray& filename)
{
    leveldb::DB* db = 0;
    leveldb::Options dbOptions;
    leveldb::WriteOptions writeOptions;
    dbOptions.create_if_missing = true;
    if (!leveldb::DB::Open(dbOptions, filename.constData(), &db).ok()) {
        return;
    }
    Q_ASSERT(db);

    foreach(const CollectData::DataEntry& entry, mData->data) {
        writeEntry(db, writeOptions, entry);
    }

    delete db;
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

static inline void addCursor(const CXCursor& cursor, CollectData::Data* data)
{
    data->cursor = CursorKey(cursor);
    CXCursor parent = cursor;
    for (;;) {
        parent = clang_getCursorSemanticParent(parent);
        CursorKey parentKey(parent);
        if (!parentKey.isValid())
            break;
        data->parents.append(parentKey);
    }
}

static CXChildVisitResult collectSymbols(CXCursor cursor, CXCursor, CXClientData client_data)
{
    CollectData* data = reinterpret_cast<CollectData*>(client_data);

    CollectData::DataEntry entry;
    addCursor(cursor, &entry.cursor);
    if (data->seen.contains(entry.cursor.cursor))
        return CXChildVisit_Recurse; // ### Continue?

    CXCursor definition = clang_getCursorDefinition(cursor);
    if (clang_isCursorDefinition(cursor) || !isValidCursor(definition))
        addCursor(clang_getCursorReferenced(cursor), &entry.reference);
    else
        addCursor(definition, &entry.reference);
    addCursor(clang_getCanonicalCursor(cursor), &entry.canonical);

    if (isValidCursor(definition))
        data->seen.insert(entry.cursor.cursor);

    data->data.append(entry);

    return CXChildVisit_Recurse;
}

void RBuild::compile(const GccArguments& arguments)
{
    CXIndex idx = clang_createIndex(0, 0);
    foreach(const Path& input, arguments.input()) {
        /*if (!input.endsWith("/RBuild.cpp")
            && !input.endsWith("/main.cpp")) {
            printf("skipping %s\n", input.constData());
            continue;
        }*/
        fprintf(stderr, "parsing %s\n", input.constData());

        const bool verbose = (getenv("VERBOSE") != 0);

        QList<QByteArray> arglist;
        arglist += arguments.arguments("-I");
        arglist += arguments.arguments("-D");
        arglist += mSysInfo.systemIncludes();
        // ### not very efficient
        QVector<const char*> argvector;
        foreach(const QByteArray& arg, arglist) {
            argvector.append(arg.constData());
            if (verbose)
                fprintf(stderr, "%s ", arg.constData());
        }
        if (verbose)
            fprintf(stderr, "\n");

        CXTranslationUnit unit = clang_parseTranslationUnit(idx, input.constData(), argvector.constData(), argvector.size(), 0, 0, 0);
        if (!unit) {
            fprintf(stderr, "Unable to parse unit for %s\n", input.constData());
            continue;
        }

        const unsigned int numDiags = clang_getNumDiagnostics(unit);
        for (unsigned int i = 0; i < numDiags; ++i) {
            CXDiagnostic diag = clang_getDiagnostic(unit, i);
            CXSourceLocation loc = clang_getDiagnosticLocation(diag);
            CXFile file;
            unsigned int line, col, off;

            clang_getInstantiationLocation(loc, &file, &line, &col, &off);
            CXString fn = clang_getFileName(file);
            CXString txt = clang_getDiagnosticSpelling(diag);
            const char* fnstr = clang_getCString(fn);

            // Suppress diagnostic messages that doesn't have a filename
            if (fnstr && (strcmp(fnstr, "") != 0))
                fprintf(stderr, "%s:%u:%u %s\n", fnstr, line, col, clang_getCString(txt));

            clang_disposeString(txt);
            clang_disposeString(fn);
            clang_disposeDiagnostic(diag);
        }

        if (!mData)
            mData = new CollectData;

        CXCursor unitCursor = clang_getTranslationUnitCursor(unit);
        clang_visitChildren(unitCursor, collectSymbols, mData);

        clang_disposeTranslationUnit(unit);
    }
    clang_disposeIndex(idx);
}
