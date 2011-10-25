#include "RBuild.h"
#include <RTags.h>
#include <QCoreApplication>
#include <QtAlgorithms>
#include <sstream>
#include <clang-c/Index.h>
#include <leveldb/db.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/types.h>
#include <dirent.h>
#include "AtomicString.h"
#include "CursorKey.h"
#include "RBuild_p.h"

using namespace RTags;

RBuild::RBuild(QObject *parent)
    : QObject(parent), mData(new RBuildPrivate)
{
}

RBuild::~RBuild()
{
    delete mData;
}

void RBuild::setDBPath(const Path &path)
{
    mDBPath = path;
    mSysInfo.init();
}

void RBuild::buildDB(const Path& makefile)
{
    mMakefile = makefile;
    startParse();
}

bool RBuild::updateDB()
{
    leveldb::DB* db = 0;
    leveldb::Options dbOptions;
    if (!leveldb::DB::Open(dbOptions, mDBPath.constData(), &db).ok()) {
        fprintf(stderr, "Can't open db [%s]\n", mDBPath.constData());
        return false;
    }
    LevelDBScope scope(db);
    QHash<Path, GccArguments> dirty;
    leveldb::Iterator* it = db->NewIterator(leveldb::ReadOptions());
    for (it->Seek("f:"); it->Valid(); it->Next()) {
        const leveldb::Slice key = it->key();
        if (strncmp(key.data(), "f:", 2))
            break;
        GccArguments args;
        time_t lastModified;
        QHash<Path, time_t> dependencies;

        const leveldb::Slice value = it->value();
        const QByteArray data = QByteArray::fromRawData(value.data(), value.size());
        QDataStream ds(data);
        ds >> args >> lastModified >> dependencies;
        const RBuildPrivate::Dependencies dep = { Path(key.data() + 2), args, lastModified, dependencies };
        if (lastModified != dep.file.lastModified())
            dirty.insert(dep.file, args);
        for (QHash<Path, time_t>::const_iterator it = dependencies.constBegin(); it != dependencies.constEnd(); ++it) {
            if (!dirty.contains(it.key()) && it.key().lastModified() != it.value())
                dirty.insert(it.key(), GccArguments());
        }

        mData->dependencies.append(dep);
        // qDebug() << file << args.raw() << ctime(&lastModified) << dependencies;
    }
    delete it;

    std::string value;
    if (!db->Get(leveldb::ReadOptions(), " ", &value).ok()) {
        fprintf(stderr, "Can't read existing data\n");
        return false;
    }
    const QByteArray data = QByteArray::fromRawData(value.c_str(), value.size());
    QDataStream ds(data);
    int count;
    ds >> count;
    for (int i=0; i<count; ++i) {
        RBuildPrivate::DataEntry *entry = new RBuildPrivate::DataEntry;
        ds >> *entry;
        mData->data.append(entry);
    }

    return true;
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
    writeData(mDBPath);
    fprintf(stderr, "All done.\n");

    qApp->quit();
}

void RBuild::makefileFileReady(const MakefileItem& file)
{
    compile(file.arguments);
}

static inline void writeDependencies(leveldb::DB* db, const leveldb::WriteOptions& opt,
                                     const Path &path, const GccArguments &args,
                                     time_t lastModified, const QHash<Path, time_t> &dependencies)
{
    QByteArray out;
    {
        QDataStream ds(&out, QIODevice::WriteOnly);
        ds << args << lastModified << dependencies;
    }
    const QByteArray p = "f:" + path;
    db->Put(opt, leveldb::Slice(p.constData(), p.size()),
            leveldb::Slice(out.constData(), out.size()));
}

static inline QByteArray makeRefValue(const RBuildPrivate::DataEntry& entry)
{
    QByteArray out;
    {
        QDataStream ds(&out, QIODevice::WriteOnly);
        ds << entry.reference.key.toString() << entry.references;
        // qDebug() << "writing out value for" << entry.key.cursor.toString()
        //          << entry.reference.key.toString() << entry.references;
        // const QByteArray v =
        // ds << QByteArray::fromRawData(&v[0], v.size()) << convertRefs(entry.references);
    }
    return out;
}

static inline void writeDict(leveldb::DB* db, const leveldb::WriteOptions& opt, const QHash<AtomicString, QSet<AtomicString> >& dict)
{
    QHash<AtomicString, QSet<AtomicString> >::const_iterator it = dict.begin();
    const QHash<AtomicString, QSet<AtomicString> >::const_iterator end = dict.end();
    while (it != end) {
        // qDebug() << it.key().toByteArray();
        std::string locs;
        const QSet<AtomicString>& set = it.value();
        QSet<AtomicString>::const_iterator dit = set.begin();
        const QSet<AtomicString>::const_iterator dend = set.end();
        while (dit != dend) {
            locs += (*dit).toByteArray().constData();
            locs += '\0';
            ++dit;
        }
        db->Put(opt, ("d:" + it.key().toByteArray()).constData(), locs);
        ++it;
    }
}

static inline void collectDict(const RBuildPrivate::DataEntry& entry, QHash<AtomicString, QSet<AtomicString> >& dict)
{
    const RBuildPrivate::Cursor* datas[] = { &entry.cursor, &entry.reference };
    for (int i = 0; i < 2; ++i) {
        const CursorKey& key = datas[i]->key;
        if (!key.isValid())
            continue;

        // qDebug() << "dict" << key;

        const int& kind = key.kind;
        if ((kind >= CXCursor_FirstRef && kind <= CXCursor_LastRef)
            || (kind >= CXCursor_FirstExpr && kind <= CXCursor_LastExpr))
            continue;

        const QList<AtomicString>& parents = datas[i]->parentNames;

        QByteArray name = key.symbolName.toByteArray();
        const QByteArray loc = key.toString();
        const AtomicString location(loc.constData(), loc.size());

        // add symbolname -> location
        dict[name].insert(location);
        int colon = name.indexOf('(');
        if (colon != -1)
            dict[name.left(colon)].insert(location);

        switch (kind) {
        case CXCursor_Namespace:
        case CXCursor_ClassDecl:
        case CXCursor_StructDecl:
        case CXCursor_FieldDecl:
        case CXCursor_CXXMethod:
        case CXCursor_Constructor:
        case CXCursor_Destructor:
            break;
        default:
            continue;
        }

        foreach(const AtomicString &cur, parents) {
            const int old = name.size();
            name.prepend("::");
            name.prepend(cur.toByteArray());
            if (colon != -1) {
                colon += (name.size() - old);
                dict[AtomicString(name.constData(), colon)].insert(location);
            }

            dict[AtomicString(name)].insert(location);
        }
    }
}

static inline void writeEntry(leveldb::DB* db, const leveldb::WriteOptions& opt,
                              const RBuildPrivate::DataEntry& entry)
{
    const CursorKey& key = entry.cursor.key;
    if (!key.isValid()) {
        return;
    }

    QByteArray k = key.toString();
    QByteArray v = makeRefValue(entry);
    db->Put(opt, std::string(k.constData(), k.size()), std::string(v.constData(), v.size()));
    // qDebug() << "writing" << k << kindToString(key.kind) << entry.references.size()
    //          << v.size() << std::string(v.constData(), v.size()).size();
}

static inline int removeDirectory(const char *path)
{
    DIR *d = opendir(path);
    size_t path_len = strlen(path);
    int r = -1;

    if (d) {
        struct dirent *p;

        r = 0;

        while (!r && (p=readdir(d))) {
            int r2 = -1;
            char *buf;
            size_t len;

            /* Skip the names "." and ".." as we don't want to recurse on them. */
            if (!strcmp(p->d_name, ".") || !strcmp(p->d_name, "..")) {
                continue;
            }

            len = path_len + strlen(p->d_name) + 2;
            buf = static_cast<char*>(malloc(len));

            if (buf) {
                struct stat statbuf;
                snprintf(buf, len, "%s/%s", path, p->d_name);
                if (!stat(buf, &statbuf)) {
                    if (S_ISDIR(statbuf.st_mode)) {
                        r2 = removeDirectory(buf);
                    } else {
                        r2 = unlink(buf);
                    }
                }

                free(buf);
            }

            r = r2;
        }

        closedir(d);
    }

    if (!r) {
        r = rmdir(path);
    }

    return r;
}

void RBuild::writeData(const QByteArray& filename)
{
    if (!mData)
        return;

    leveldb::DB* db = 0;
    leveldb::Options dbOptions;
    leveldb::WriteOptions writeOptions;
    dbOptions.create_if_missing = true;

    // Q_ASSERT(filename.endsWith(".rtags.db"));
    removeDirectory(filename.constData());
    if (!leveldb::DB::Open(dbOptions, filename.constData(), &db).ok()) {
        return;
    }
    Q_ASSERT(db);

    QHash<AtomicString, QSet<AtomicString> > dict;
    foreach(RBuildPrivate::DataEntry* entry, mData->data) {
        const CursorKey key = entry->cursor.key;
        const CursorKey ref = entry->reference.key;
        if (key.kind == CXCursor_CXXMethod
            || key.kind == CXCursor_Constructor
            || key.kind == CXCursor_Destructor) {
            if (key != ref && !key.isDefinition()) {
                RBuildPrivate::DataEntry *def = mData->seen.value(ref.locationKey());
                Q_ASSERT(def && def != entry);
                def->reference = entry->cursor;
            }
            continue;
        }

        RBuildPrivate::DataEntry *r = mData->seen.value(entry->reference.key.locationKey());
        if (r == entry)
            continue;
        if (r) {
            r->references.insert(entry->cursor.key);
        }
    }

    QByteArray entries;
    QDataStream ds(&entries, QIODevice::WriteOnly);
    ds << mData->data.size();
    foreach(const RBuildPrivate::DataEntry* entry, mData->data) {
        writeEntry(db, writeOptions, *entry);
        collectDict(*entry, dict);
        ds << *entry;
    }

    writeDict(db, writeOptions, dict);

    foreach(const RBuildPrivate::Dependencies &dep, mData->dependencies) {
        writeDependencies(db, writeOptions, dep.file, dep.arguments,
                          dep.lastModified, dep.dependencies);
    }

    db->Put(writeOptions, " ", leveldb::Slice(entries.constData(), entries.size()));
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
    fprintf(out, "cursor name %s, kind %s%s, loc %s:%u:%u\n",
            clang_getCString(name), clang_getCString(kind),
            cursorDefinition(cursor) ? " def" : "",
            clang_getCString(filename), line, col);
    clang_disposeString(name);
    clang_disposeString(kind);
    clang_disposeString(filename);
}

static inline void addCursor(const CXCursor& cursor, const CursorKey& key, RBuildPrivate::Cursor* data)
{
    Q_ASSERT(key.isValid());
    data->key = key;
    CXCursor parent = cursor;
    for (;;) {
        parent = clang_getCursorSemanticParent(parent);
        CursorKey parentKey(parent);
        if (!parentKey.isValid())
            break;
        switch (parentKey.kind) {
        case CXCursor_StructDecl:
        case CXCursor_ClassDecl:
        case CXCursor_Namespace:
            Q_ASSERT(!parentKey.symbolName.isEmpty());
            data->parentNames.append(parentKey.symbolName);
            break;
        default:
            break;
        }
    }
}

//#define REFERENCEDEBUG

static inline bool useCursor(CXCursorKind kind)
{
    switch (kind) {
    case CXCursor_CallExpr:
        return false;
    default:
        break;
    }
    return true;
}

static inline CXCursor referencedCursor(const CXCursor& cursor)
{
#ifdef REFERENCEDEBUG
    CursorKey key(cursor);
    const bool dodebug = (key.fileName.toByteArray().endsWith("GccArguments.cpp") && key.line == 74);
#endif

    CXCursor ret;
    const CXCursorKind kind = clang_getCursorKind(cursor);

    if (!useCursor(kind)) {
#ifdef REFERENCEDEBUG
        if (dodebug) {
            printf("making ref, throwing out\n");
            debugCursor(stdout, cursor);
        }
#endif
        return clang_getNullCursor();
    }

    if (kind >= CXCursor_FirstRef && kind <= CXCursor_LastRef) {
#ifdef REFERENCEDEBUG
        if (dodebug) {
            printf("making ref, ref\n");
            debugCursor(stdout, cursor);
        }
#endif
        const CXType type = clang_getCursorType(cursor);
        if (type.kind == CXType_Invalid)
            ret = clang_getCursorReferenced(cursor);
        else
            ret = clang_getTypeDeclaration(type);
        if (isValidCursor(ret)) {
#ifdef REFERENCEDEBUG
            if (dodebug)
                debugCursor(stdout, ret);
#endif
        } else
            ret = cursor;
    } else if (kind >= CXCursor_FirstExpr && kind <= CXCursor_LastExpr) {
#ifdef REFERENCEDEBUG
        if (dodebug) {
            printf("making ref, expr\n");
            debugCursor(stdout, cursor);
        }
#endif
        ret = clang_getCursorReferenced(cursor);
#ifdef REFERENCEDEBUG
        if (dodebug)
            debugCursor(stdout, ret);
#endif
    } else if (kind >= CXCursor_FirstStmt && kind <= CXCursor_LastStmt) {
#ifdef REFERENCEDEBUG
        if (dodebug) {
            printf("making ref, stmt\n");
            debugCursor(stdout, cursor);
        }
#endif
        ret = clang_getCursorReferenced(cursor);
        if (isValidCursor(ret)) {
#ifdef REFERENCEDEBUG
            if (dodebug)
                debugCursor(stdout, ret);
#endif
        } else
            ret = cursor;
    } else if (kind >= CXCursor_FirstDecl && kind <= CXCursor_LastDecl) {
#ifdef REFERENCEDEBUG
        if (dodebug) {
            printf("making ref, decl\n");
            debugCursor(stdout, cursor);
        }
#endif
        ret = clang_getCursorReferenced(cursor);
#ifdef REFERENCEDEBUG
        if (dodebug)
            debugCursor(stdout, ret);
#endif
    } else if (kind == CXCursor_MacroDefinition || kind == CXCursor_MacroExpansion) {
#ifdef REFERENCEDEBUG
        if (dodebug) {
            printf("making ref, macro\n");
            debugCursor(stdout, cursor);
        }
#endif
        if (kind == CXCursor_MacroExpansion) {
            ret = clang_getCursorReferenced(cursor);
#ifdef REFERENCEDEBUG
            if (dodebug)
                debugCursor(stdout, ret);
#endif
        } else
            ret = cursor;
    } else {
#ifdef REFERENCEDEBUG
        if (!key.symbolName.isEmpty()) {
            if (kind != CXCursor_InclusionDirective) {
                fprintf(stderr, "unhandled reference %s\n", eatString(clang_getCursorKindSpelling(clang_getCursorKind(cursor))).constData());
                debugCursor(stderr, cursor);
            }
        }
#endif
        ret = clang_getNullCursor();
    }
    return ret;
}

static inline bool equalLocation(const CursorKey& key1, const CursorKey& key2)
{
    return (key1.off == key2.off && key1.fileName == key2.fileName);
}

// #define COLLECTDEBUG

static CXChildVisitResult collectSymbols(CXCursor cursor, CXCursor, CXClientData client_data)
{
    const CursorKey key(cursor);
    if (!key.isValid())
        return CXChildVisit_Recurse;
    // qDebug() << key << kindToString(key.kind);

    RBuildPrivate* data = reinterpret_cast<RBuildPrivate*>(client_data);

    RBuildPrivate::DataEntry* entry = 0;
    const QHash<QByteArray, RBuildPrivate::DataEntry*>::const_iterator it = data->seen.find(key.locationKey());
    static const bool verbose = getenv("VERBOSE");
    if (verbose) {
        debugCursor(stderr, cursor);
    }

#ifdef COLLECTDEBUG
    const bool dodebug = (key.fileName.toByteArray().endsWith("main.cpp") && key.line == 10 && key.col == 6);
#endif
    if (it != data->seen.end()) {
        entry = it.value();
        if (entry->hasDefinition) {
#ifdef COLLECTDEBUG
            if (dodebug) {
                fprintf(stdout, "already got a def\n");
                qDebug() << key;
            }
#endif
            return CXChildVisit_Recurse; // ### Continue?
        }
    } else {
        entry = new RBuildPrivate::DataEntry;
        data->seen[key.locationKey()] = entry;
        data->data.append(entry);
    }
    // if (!entry->refData)
    //     entry->refData = findRefData(data, cursor);

    if (key.kind == CXCursor_InclusionDirective) {
        CursorKey inclusion;
        inclusion.fileName = eatString(clang_getFileName(clang_getIncludedFile(cursor)));
        inclusion.symbolName = inclusion.fileName;
        inclusion.line = inclusion.col = 1;
        inclusion.off = 0;
        addCursor(cursor, key, &entry->cursor);
        addCursor(clang_getNullCursor(), inclusion, &entry->reference);
        entry->hasDefinition = true;
        return CXChildVisit_Continue;
    }

    const CXCursor definition = clang_getCursorDefinition(cursor);
#ifdef COLLECTDEBUG
    if (dodebug) {
        debugCursor(stdout, cursor);
        debugCursor(stdout, definition);
        fprintf(stdout, "(%d %d)\n", !isValidCursor(definition), equalLocation(key, CursorKey(definition)));
    }
#endif
    if (!cursorDefinition(definition) || equalLocation(key, CursorKey(definition))) {
        if (entry->reference.key.isNull() || entry->reference.key == entry->cursor.key) {
            const CXCursor reference = clang_getCursorReferenced(cursor);
            const CursorKey referenceKey(reference);
            if (referenceKey.isValid()/* && referenceKey != key*/) {
#ifdef COLLECTDEBUG
                if (dodebug) {
                    debugCursor(stdout, reference);
                    fprintf(stdout, "ref %p\n", entry);
                }
#endif
                addCursor(cursor, key, &entry->cursor);
                addCursor(reference, referenceKey, &entry->reference);
            }
        }
    } else {
        if (cursorDefinitionFor(definition, cursor))
            entry->hasDefinition = true;
        addCursor(cursor, key, &entry->cursor);
        const CursorKey definitionKey(definition);
        if (definitionKey.isValid()) {
            addCursor(definition, definitionKey, &entry->reference);
        }
#ifdef COLLECTDEBUG
        if (dodebug) {
            debugCursor(stdout, definition);
            fprintf(stdout, "def %p\n", entry);
            qDebug() << entry->reference.cursor;
        }
#endif
    }

    return CXChildVisit_Recurse;
}

static inline void getInclusions(CXFile includedFile,
                                 CXSourceLocation* inclusionStack,
                                 unsigned evilUnsigned,
                                 CXClientData userData)
{
    int includeLen = evilUnsigned;
    if (includeLen) {
        RBuildPrivate::Dependencies *deps = reinterpret_cast<RBuildPrivate::Dependencies*>(userData);
        CXString str = clang_getFileName(includedFile);
        Path p = Path::resolved(clang_getCString(str));
        deps->dependencies[p] = p.lastModified();
        clang_disposeString(str);
        // printf("Included file %s %d\n", eatString(clang_getFileName(includedFile)).constData(), includeLen);
        // qDebug() << includeLen;
        for (int i=0; i<includeLen - 1; ++i) {
            CXFile f;
            clang_getSpellingLocation(inclusionStack[i], &f, 0, 0, 0);
            str = clang_getFileName(f);
            p = Path::resolved(clang_getCString(str));
            deps->dependencies[p] = p.lastModified();
            clang_disposeString(str);
            // printf("    %d %s\n", i, eatString(clang_getFileName(f)).constData());
        }
    }
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

        CXTranslationUnit unit = clang_parseTranslationUnit(idx, input.constData(),
                                                            argvector.constData(), argvector.size(),
                                                            0, 0,
                                                            CXTranslationUnit_DetailedPreprocessingRecord);
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

        CXCursor unitCursor = clang_getTranslationUnitCursor(unit);
        clang_visitChildren(unitCursor, collectSymbols, mData);
        RBuildPrivate::Dependencies deps = { input, arguments, input.lastModified(),
                                             QHash<Path, time_t>() };
        mData->dependencies.append(deps);
        clang_getInclusions(unit, getInclusions, &mData->dependencies.last());
        clang_disposeTranslationUnit(unit);
    }
    clang_disposeIndex(idx);
}
