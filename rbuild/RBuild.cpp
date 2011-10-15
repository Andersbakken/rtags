#include "RBuild.h"
#include <QCoreApplication>
#include <QtAlgorithms>
#include <sstream>
#include <clang-c/Index.h>
#include <leveldb/db.h>
#include <stdio.h>
#include <stdint.h>

//#define REENTRANT_ATOMICSTRING

class AtomicString
{
public:
    AtomicString() : mData(0) {}
    AtomicString(const CXString& string);
    AtomicString(const QByteArray& string);
    AtomicString(const QString& string);
    AtomicString(const AtomicString& other);
    ~AtomicString();

    AtomicString& operator=(const AtomicString& other);
    QByteArray operator*() const { return mData ? mData->data : QByteArray(); }
    bool operator==(const AtomicString& other) const { return mData == other.mData; }
    bool operator==(const QByteArray& string) const { return mData ? mData->data == string : false; }
    bool operator!=(const QByteArray& string) const { return mData ? mData->data != string : false; }
    bool operator<(const QByteArray& string) const { return mData ? mData->data < string : false; }
    bool operator>(const QByteArray& string) const { return mData ? mData->data > string : false; }
    bool operator<=(const QByteArray& string) const { return mData ? mData->data <= string : false; }
    bool operator>=(const QByteArray& string) const { return mData ? mData->data >= string : false; }

    bool isEmpty() const { return mData ? mData->data.isEmpty() : true; }
    int strcmp(const AtomicString& other) const;

    QByteArray toByteArray() const { return mData ? mData->data : QByteArray(); }
    QString toString() const { return QString::fromUtf8(mData ? mData->data.constData() : 0); }

private:
    void init(const QByteArray& str);

private:
    struct Data
    {
        QByteArray data;
        int ref;
    };

    Data* mData;

    static QHash<QByteArray, Data*> sData;
#ifdef REENTRANT_ATOMICSTRING
    static QMutex sMutex;
#endif
};

QHash<QByteArray, AtomicString::Data*> AtomicString::sData;
#ifdef REENTRANT_ATOMICSTRING
QMutex AtomicString::sMutex;
#endif

inline void AtomicString::init(const QByteArray& str)
{
    QHash<QByteArray, Data*>::iterator it = sData.find(str);
    if (it != sData.end()) {
        mData = it.value();
        ++mData->ref;
    } else {
        mData = new Data;
        mData->data = str;
        mData->ref = 1;
        sData[str] = mData;
    }
}

AtomicString::AtomicString(const CXString& string)
{
#ifdef REENTRANT_ATOMICSTRING
    QMutexLocker locker(&sMutex);
#endif
    QByteArray ba(clang_getCString(string));
    init(ba);
}

AtomicString::AtomicString(const QByteArray& string)
{
#ifdef REENTRANT_ATOMICSTRING
    QMutexLocker locker(&sMutex);
#endif
    init(string);
}

AtomicString::AtomicString(const QString& string)
{
#ifdef REENTRANT_ATOMICSTRING
    QMutexLocker locker(&sMutex);
#endif
    init(string.toUtf8());
}

AtomicString::AtomicString(const AtomicString& other)
{
#ifdef REENTRANT_ATOMICSTRING
    QMutexLocker locker(&sMutex);
#endif
    mData = other.mData;
    if (mData)
        ++mData->ref;
}

AtomicString::~AtomicString()
{
#ifdef REENTRANT_ATOMICSTRING
    QMutexLocker locker(&sMutex);
#endif
    if (mData && !--mData->ref) {
        sData.remove(mData->data);
        delete mData;
        mData = 0;
    }
}

int AtomicString::strcmp(const AtomicString& other) const
{
#ifdef REENTRANT_ATOMICSTRING
    QMutexLocker locker(&sMutex);
#endif
    if (!mData)
        return !other.mData ? 0 : -1;
    if (!other.mData)
        return 1;
    return qstrcmp(mData->data, other.mData->data);
}

AtomicString& AtomicString::operator=(const AtomicString& other)
{
#ifdef REENTRANT_ATOMICSTRING
    QMutexLocker locker(&sMutex);
#endif
    if (mData && !--mData->ref) {
        sData.remove(mData->data);
        delete mData;
    }
    mData = other.mData;
    if (mData)
        ++mData->ref;
    return *this;
}

static inline uint qHash(const AtomicString& string)
{
    return qHash(string.toByteArray());
}

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
        int ret = fileName.strcmp(other.fileName);
        if (ret < 0)
            return true;
        if (ret > 0)
            return false;
        if (off < other.off)
            return true;
        if (off > other.off)
            return false;
        ret = symbolName.strcmp(other.symbolName);
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
    AtomicString fileName;
    AtomicString symbolName;
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

        QByteArray name = key.fileName.toByteArray();
        const char *ch = name.constData();
        Q_ASSERT(ch);
        while (*ch) {
            HASHCHAR(*ch);
            ++ch;
        }
        name = key.symbolName.toByteArray();
        ch = name.constData();
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
    CollectData() {}
    ~CollectData() { qDeleteAll(data); }

    struct Data
    {
        CursorKey cursor;
        QList<CursorKey> parents;
    };
    struct DataEntry
    {
        bool hasDefinition;
        Data cursor;
        Data reference;
        Data canonical;
    };

    QHash<CursorKey, DataEntry*> seen;
    QList<DataEntry*> data;
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
    stream << key.fileName.toByteArray().constData();
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

    foreach(const CollectData::DataEntry* entry, mData->data) {
        writeEntry(db, writeOptions, *entry);
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

static inline void addCursor(const CXCursor& cursor, const CursorKey& key, CollectData::Data* data)
{
    data->cursor = key;
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

    CursorKey key(cursor);
    CollectData::DataEntry* entry = 0;
    QHash<CursorKey, CollectData::DataEntry*>::iterator it = data->seen.find(key);
    if (it != data->seen.end()) {
        entry = it.value();
        if (entry->hasDefinition)
            return CXChildVisit_Recurse; // ### Continue?
    } else {
        entry = new CollectData::DataEntry;
        entry->hasDefinition = false;
        data->seen[key] = entry;
        data->data.append(entry);
        addCursor(cursor, key, &entry->cursor);
        // ### not used yet so don't add for now
        // addCursor(clang_getCanonicalCursor(cursor), &entry->canonical);
    }

    CXCursor definition = clang_getCursorDefinition(cursor);
    if (clang_isCursorDefinition(cursor) || !isValidCursor(definition)) {
        if (entry->reference.cursor.isNull()) {
            CXCursor reference = clang_getCursorReferenced(cursor);
            addCursor(reference, CursorKey(reference), &entry->reference);
        }
    } else {
        entry->hasDefinition = true;
        addCursor(definition, CursorKey(definition), &entry->reference);
    }

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
