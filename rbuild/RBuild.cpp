#include "RBuild.h"
#include "Precompile.h"
#include <RTags.h>
#include <QCoreApplication>
#include <QtAlgorithms>
#include <sstream>
#include <clang-c/Index.h>
#include <leveldb/db.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/types.h>
#include <errno.h>
#include <dirent.h>
#include "AtomicString.h"
#include "CursorKey.h"
#include "RBuild_p.h"
#include <leveldb/write_batch.h>
#include <memory>

using namespace RTags;

static const bool pchEnabled = !getenv("RTAGS_NO_PCH");
static QElapsedTimer timer;
static int symbols = 0;
static int validSymbols = 0;
static int visitedSymbols = 0;

RBuild::RBuild(QObject *parent)
    : QObject(parent), mData(new RBuildPrivate), mIndex(clang_createIndex(1, 1))
{
    timer.start();
}

RBuild::~RBuild()
{
    clang_disposeIndex(mIndex);
    delete mData;
}

void RBuild::setDBPath(const Path &path)
{
    mDBPath = path;
    mSysInfo.init();
}

bool RBuild::buildDB(const Path& makefile, const Path &sourceDir)
{
    if (!makefile.exists()) {
        fprintf(stderr, "%s doesn't exist\n", makefile.constData());
        return false;
    }
    mMakefile = makefile;
    mSourceDir = sourceDir;
    if (!mSourceDir.isEmpty()) {
        mSourceDir.resolve();
        if (!mSourceDir.isDir()) {
            fprintf(stderr, "%s is not a directory\n", sourceDir.constData());
            return false;
        }
        if (!mSourceDir.endsWith('/'))
            mSourceDir.append('/');
    }

    startParse();
    return true;
}

static inline bool contains(const QHash<Path, GccArguments> &dirty, const AtomicString &fileName)
{
    const Path p = QByteArray::fromRawData(fileName.constData(), fileName.size());
    return dirty.contains(p);
}

static inline bool filter(RBuildPrivate::DataEntry &entry, const QHash<Path, GccArguments> &dirty)
{
    if (::contains(dirty, entry.cursor.key.fileName)
        || ::contains(dirty, entry.reference.key.fileName)) {
        return false;
    }
    QSet<Cursor>::iterator it = entry.references.begin();
    while (it != entry.references.end()) {
        if (::contains(dirty, (*it).key.fileName)) {
            // qDebug() << "throwing out reference" << (*it).key;
            it = entry.references.erase(it);
        } else {
            ++it;
        }
    }

    return true;
}

static inline int fileNameLength(const char *data, int len)
{
    Q_ASSERT(len > 1);
    const char *c = data + len - 1;
    int colons = 3;
    forever {
        if (*c == ':' && !--colons)
            break;
        --c;
        Q_ASSERT(c != data);
    }
    return (c - data);
}

bool RBuild::updateDB()
{
    const qint64 beforeLoad = timer.elapsed();
    leveldb::DB* db = 0;
    leveldb::Options dbOptions;
    if (!leveldb::DB::Open(dbOptions, mDBPath.constData(), &db).ok()) {
        fprintf(stderr, "Can't open db [%s]\n", mDBPath.constData());
        return false;
    }
    LevelDBScope scope(db);
    QHash<Path, GccArguments> dirty;
    int dirtySourceFiles = 0;
    std::auto_ptr<leveldb::Iterator> it(db->NewIterator(leveldb::ReadOptions()));
    for (it->Seek("f:"); it->Valid(); it->Next()) {
        const leveldb::Slice key = it->key();
        if (strncmp(key.data(), "f:", 2))
            break;
        GccArguments args;
        quint64 lastModified;
        QHash<Path, quint64> dependencies;

        const leveldb::Slice value = it->value();
        const QByteArray data = QByteArray::fromRawData(value.data(), value.size());
        QDataStream ds(data);
        ds >> args >> lastModified >> dependencies;
        const Path file(key.data() + 2, key.size() - 2);
        bool recompile = false;
        if (lastModified != file.lastModified()) {
            recompile = true;
            // quint64 lm = dep.file.lastModified();
            // qDebug() << dep.file << "has changed" << ctime(&lm) << ctime(&lastModified);
        } else {
            for (QHash<Path, quint64>::const_iterator it = dependencies.constBegin(); it != dependencies.constEnd(); ++it) {
                if (dirty.contains(it.key())) {
                    recompile = true;
                    break;
                } else if (it.key().lastModified() != it.value()) {
                    dirty.insert(it.key(), GccArguments());
                    recompile = true;
                    break;
                }
            }
        }

        if (recompile) {
            ++dirtySourceFiles;
            dirty.insert(file, args);
        }
        // qDebug() << file << args.raw() << ctime(&lastModified) << dependencies;
    }
    if (!dirtySourceFiles) {
        printf("Nothing has changed (%lld ms)\n", timer.elapsed());
        return true;
    }
    if (!readFromDB(db, "sourceDir", mSourceDir)) {
        fprintf(stderr, "Can't read existing data for src dir\n");
        return false;
    }
    
    leveldb::WriteBatch batch;
    for (it->Seek("/"); it->Valid(); it->Next()) {
        const leveldb::Slice key = it->key();
        Q_ASSERT(!key.empty());
        if (key.data()[0] != '/')
            break;
        const Path p = QByteArray::fromRawData(key.data(), fileNameLength(key.data(), key.size()));
        if (dirty.contains(p)) {
            batch.Delete(key);
            // qDebug() << "ditching" << QByteArray::fromRawData(key.data(), key.size());
            continue;
        }
        const leveldb::Slice value = it->value();
        const QByteArray v = QByteArray::fromRawData(value.data(), value.size());
        QDataStream ds(v);
        QByteArray referredTo;
        ds >> referredTo; // read referred to
        QSet<Cursor> references;
        ds >> references;
        bool changed = false;
        // qDebug() << "looking at key" << QByteArray::fromRawData(key.data(), key.size()) << references.size();

        QSet<Cursor>::iterator sit = references.begin();
        while (sit != references.end()) {
            const QByteArray fileName = (*sit).key.fileName.toByteArray();
            if (dirty.contains(*static_cast<const Path*>(&fileName))) {
                // qDebug() << "ditched reference to" << key << "from" << (*sit).key;
                sit = references.erase(sit);
                changed = true;
            } else {
                ++sit;
            }
        }
        if (changed) {
            // ### could really hang on to this whole thing since we're
            // ### quite likely to make an additional change to it
            QByteArray out;
            {
                QDataStream d(&out, QIODevice::WriteOnly);
                d << referredTo << references;
            }
            // qDebug() << "writing to key" << key;
            batch.Put(key, leveldb::Slice(out.constData(), out.size()));
        }
    }
    for (it->Seek("d:"); it->Valid(); it->Next()) {
        const leveldb::Slice key = it->key();
        Q_ASSERT(!key.empty());
        if (strncmp(key.data(), "d:", 2))
            break;
        QSet<AtomicString> locations = readFromSlice<QSet<AtomicString> >(it->value());
        bool foundDirty = false;
        QSet<AtomicString>::iterator it = locations.begin();
        while (it != locations.end()) {
            const AtomicString &k = (*it);
            const Path p = QByteArray::fromRawData(k.constData(), fileNameLength(k.constData(), k.size()));
            if (dirty.contains(p)) {
                foundDirty = true;
                // qDebug() << "ditching" << k;
                it = locations.erase(it);
            } else {
                ++it;
            }
        }
        if (foundDirty) {
            // qDebug() << "Found dirty for" << QByteArray::fromRawData(key.data(), key.size());
            if (locations.isEmpty()) {
                batch.Delete(key);
            } else {
                writeToBatch(&batch, key, locations);
            }

        }
    }

    // return true;
    // qDebug() << dirty;

    bool pchDirty = false;
    {
        std::string value;
        if (db->Get(leveldb::ReadOptions(), "pch", &value).ok()) {
            const QByteArray data = QByteArray::fromRawData(value.c_str(), value.size());
            QDataStream ds(data);
            int pchCount;
            ds >> pchCount;
            for (int i=0; i<pchCount; ++i) {
                Path pch, header;
                GccArguments args;
                QHash<Path, quint64> dependencies;
                ds >> pch >> header >> args >> dependencies;
                if (pch.exists() && header.exists()) {
                    bool ok = true;
                    for (QHash<Path, quint64>::const_iterator it = dependencies.constBegin();
                         it != dependencies.constEnd(); ++it) {
                        if (dirty.contains(it.key())) {
                            ok = false;
                            break;
                        } else if (it.key().lastModified() != it.value()) {
                            dirty.insert(it.key(), GccArguments());
                            ok = false;
                            break;
                        }
                    }
                    if (ok) {
                        Precompile::create(args, pch, header, dependencies);
                    } else {
                        pchDirty = false;
                    }
                }
            }
        }
    }

    printf("Loading data took %lld ms\n", timer.elapsed() - beforeLoad);

    for (QHash<Path, GccArguments>::const_iterator it = dirty.begin(); it != dirty.end(); ++it) {
        const GccArguments &args = it.value();
        if (args.isCompile()) {
            processFile(args);
        }
    }
    unsigned writeDataFlags = LookupReferencesFromDatabase;
    if (pchDirty) {
        precompileAll();
    } else {
        writeDataFlags |= ExcludePCH;
    }
    compileAll();

    // if (count != mData->data.size())
    //     fprintf(stderr, "Item count changed from %d to %d\n",
    //             count, mData->data.size());

    writeData(db, &batch, writeDataFlags);
    db->Write(leveldb::WriteOptions(), &batch);
    printf("Updated db %lld ms\n", timer.elapsed());
    return true;
}

void RBuild::startParse()
{
    connect(&mParser, SIGNAL(fileReady(const GccArguments&)),
            this, SLOT(processFile(const GccArguments&)));
    connect(&mParser, SIGNAL(done()), this, SLOT(makefileDone()));
    mParser.run(mMakefile);
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

void RBuild::save()
{
    fprintf(stderr, "Done parsing, now writing.\n");
    const qint64 beforeWriting = timer.elapsed();

    leveldb::DB *db = 0;
    leveldb::Options dbOptions;
    leveldb::WriteOptions writeOptions;
    dbOptions.create_if_missing = true;
    removeDirectory(mDBPath.constData());
    // Q_ASSERT(filename.endsWith(".rtags.db"));
    if (!leveldb::DB::Open(dbOptions, mDBPath.constData(), &db).ok()) {
        return;
    }
    leveldb::WriteBatch batch;
    writeData(db, &batch, 0);
    db->Write(leveldb::WriteOptions(), &batch);
    delete db;
    const qint64 elapsed = timer.elapsed();
    fprintf(stderr, "All done. (total/saving %lld/%lld ms)\n", elapsed, elapsed - beforeWriting);
    qApp->quit();
}

void RBuild::compileAll()
{
    int i = 0;
    foreach(const GccArguments& args, mFiles) {
        const int old = mData->data.size();
        const qint64 before = timer.elapsed();
        bool usedPch;
        symbols = validSymbols = visitedSymbols = 0;
        compile(args, &usedPch);
        const qint64 elapsed = timer.elapsed();
        fprintf(stderr, "parsed %s (%d/%d), %d new items (%lld ms) (%d, %d, %d) %s\n",
                args.input().value(0).constData(), ++i, mFiles.size(),
                mData->data.size() - old, elapsed - before,
                symbols, validSymbols, visitedSymbols, usedPch ? "pch" : "");

    }
    mFiles.clear();
}

static void collectHeaders(const GccArguments& arguments)
{
    if (!arguments.isCompile())
        return;
    Precompile* pre = Precompile::precompiler(arguments);
    Q_ASSERT(pre);
    QList<Path> inputs = arguments.input();
    Q_ASSERT(inputs.size() == 1);
    const Path& input = inputs.front();
    QFile f(input);
    if (!f.open(QFile::ReadOnly)) {
        qWarning("unable to open file '%s' for header collection", input.constData());
        return;
    }

    QByteArray unsaved;
    unsaved.reserve(f.size() / 2);
    const QList<QByteArray> lines = f.readAll().split('\n');
    for (int i=0; i<lines.size(); ++i) {
        QByteArray line = lines.at(i).trimmed();
        if (line.startsWith('#')) {
            while (line.endsWith('\\') && i + 1 < lines.size()) {
                line.chop(1);
                line += lines.at(++i).trimmed();
            }
            if (line.startsWith("#include ")) {
                line.remove(0, 9);
            }

            unsaved.append(line);
            unsaved.append('\n');
        }
    }

    pre->addData(unsaved, input.parentDir());
}

void RBuild::processFile(const GccArguments& arguments)
{
    if (pchEnabled && !Precompile::precompiler(arguments)->isCompiled())
        collectHeaders(arguments);
    mFiles.append(arguments);
}

void RBuild::makefileDone()
{
    if (pchEnabled)
        precompileAll();
    compileAll();
    save();
}

static inline void writeDependencies(leveldb::WriteBatch* batch, const Path &path, const GccArguments &args,
                                     quint64 lastModified, const QHash<Path, quint64> &dependencies,
                                     QSet<Path> *allFiles)
{
    QByteArray out;
    {
        QDataStream ds(&out, QIODevice::WriteOnly);
        ds << args << lastModified << dependencies;
    }
    if (allFiles) {
        for (QHash<Path, quint64>::const_iterator it = dependencies.begin(); it != dependencies.end(); ++it) {
            allFiles->insert(it.key());
        }
        allFiles->insert(path);
    }

    const QByteArray p = "f:" + path;
    batch->Put(leveldb::Slice(p.constData(), p.size()),
               leveldb::Slice(out.constData(), out.size()));
}

static inline QByteArray makeRefValue(const RBuildPrivate::DataEntry& entry)
{
    QByteArray out;
    const bool refersToSelf = entry.reference.key == entry.cursor.key;
    if (refersToSelf && entry.references.isEmpty()) {
        // qDebug() << "should not be written" << entry.cursor.key;
        return out;
    }
    {
        QDataStream ds(&out, QIODevice::WriteOnly);
        if (!refersToSelf) {
            ds << entry.reference.key.toString();
        } else {
            ds << QByteArray();
        }
        ds << entry.references;
        // qDebug() << "writing out value for" << entry.key.cursor.toString()
        //          << entry.reference.key.toString() << entry.references;
        // const QByteArray v =
        // ds << QByteArray::fromRawData(&v[0], v.size()) << convertRefs(entry.references);
    }
    return out;
}

static inline void writeDict(leveldb::WriteBatch* batch, const QHash<AtomicString, QSet<AtomicString> >& dict)
{
    QHash<AtomicString, QSet<AtomicString> >::const_iterator it = dict.begin();
    const QHash<AtomicString, QSet<AtomicString> >::const_iterator end = dict.end();
    while (it != end) {
        writeToBatch(batch, ("d:" + it.key().toByteArray()), it.value());
        ++it;
    }
}

static inline void collectDict(const RBuildPrivate::DataEntry& entry, QHash<AtomicString, QSet<AtomicString> >& dict)
{
    const Cursor* datas[] = { &entry.cursor, &entry.reference };
    for (int i = 0; i < 2; ++i) {
        const CursorKey& key = datas[i]->key;
        if (!key.isValid())
            continue;

        // qDebug() << "dict" << key;

        const int& kind = key.kind;
        if ((kind >= CXCursor_FirstRef && kind <= CXCursor_LastRef)
            || (kind >= CXCursor_FirstExpr && kind <= CXCursor_LastExpr))
            continue;

        const QVector<AtomicString>& parents = datas[i]->parentNames;

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

        // qDebug() << name << parents;
        foreach(const AtomicString &cur, parents) {
            const int old = name.size();
            name.prepend("::");
            name.prepend(cur.toByteArray());
            if (colon != -1) {
                colon += (name.size() - old);
                dict[AtomicString(name.constData(), colon)].insert(location);
            }

            // qDebug() << "inserting" << name;
            dict[AtomicString(name)].insert(location);
        }
    }
}

static inline void writeEntry(leveldb::WriteBatch* batch, const RBuildPrivate::DataEntry& entry)
{
    const CursorKey& key = entry.cursor.key;
    if (!key.isValid()) {
        return;
    }

    QByteArray k = key.toString();
    QByteArray v = makeRefValue(entry);
    if (!v.isEmpty())
        batch->Put(leveldb::Slice(k.constData(), k.size()), leveldb::Slice(v.constData(), v.size()));
    // qDebug() << "writing" << k << kindToString(key.kind) << entry.references.size()
    //          << v.size() << std::string(v.constData(), v.size()).size();
}

static void recurseDir(QSet<Path> *allFiles, Path path, int rootDirLen)
{
#if defined(_DIRENT_HAVE_D_TYPE) || defined(Q_OS_BSD4) || defined(Q_OS_SYMBIAN)
    DIR *d = opendir(path.constData());
    char fileBuffer[PATH_MAX];
    if (d) {
        if (!path.endsWith('/'))
            path.append('/');
        dirent *p;
        while ((p=readdir(d))) {
            switch (p->d_type) {
            case DT_DIR:
                if (p->d_name[0] != '.') {
                    recurseDir(allFiles, path + QByteArray::fromRawData(p->d_name, strlen(p->d_name)), rootDirLen);
                }
                break;
            case DT_REG: {
                const int w = snprintf(fileBuffer, PATH_MAX, "%s%s", path.constData() + rootDirLen, p->d_name);
                if (w >= PATH_MAX) {
                    fprintf(stderr, "Path too long: %d, max is %d\n", w, PATH_MAX);
                } else {
                    allFiles->insert(Path(fileBuffer, w));
                }
                break; }
                // case DT_LNK: not following links
            }

        }
        closedir(d);
    }
#else
#warning "Can't use --source-dir on this platform"
#endif
}

void RBuild::writeData(leveldb::DB *db, leveldb::WriteBatch *batch, unsigned flags)
{
    if (!mData)
        return;

    Q_ASSERT(batch);

    QHash<AtomicString, QSet<AtomicString> > dict;
    foreach(RBuildPrivate::DataEntry* entry, mData->data) {
        const CursorKey key = entry->cursor.key;
        const CursorKey ref = entry->reference.key;
        if (key.kind == CXCursor_CXXMethod
            || key.kind == CXCursor_Constructor
            || key.kind == CXCursor_Destructor) {
            if (key != ref && !key.isDefinition()) {
                RBuildPrivate::DataEntry *def = mData->seen.value(ref.locationKey());
                if (!def) {
                    qDebug() << "no def for" << key.toString() << ref.toString();
                } else if (def == entry) {
                    qDebug() << "wrong def for" << ref.toString()
                             << "got" << entry->cursor.key.toString();
                }

                Q_ASSERT(def && def != entry);
                def->reference = entry->cursor;
            }
            continue;
        }

        const QByteArray refKey = ref.locationKey();
        // qWarning() << entry->cursor.key << "references" << entry->reference.key;
        RBuildPrivate::DataEntry *r = mData->seen.value(refKey);
        // if (flags & LookupReferencesFromDatabase) {
        //     qDebug() << key << ref << r;
        // }
        if (!r && (flags & LookupReferencesFromDatabase)) {
            std::auto_ptr<leveldb::Iterator> it(db->NewIterator(leveldb::ReadOptions()));
            it->Seek(refKey.constData());
            if (it->Valid()) {
                leveldb::Slice val = it->value();
                QByteArray data(val.data(), val.size());
                {
                    QDataStream ds(&data, QIODevice::ReadWrite);
                    QByteArray mapsTo;
                    ds >> mapsTo;
                    const int pos = ds.device()->pos();
                    QSet<Cursor> refs;
                    ds >> refs;
                    refs.insert(entry->cursor);
                    ds.device()->seek(pos);
                    ds << refs;
                }
                batch->Put(it->key(), leveldb::Slice(data.constData(), data.size()));
                // qDebug() << "successfully looked up" << refKey << "and added ref" << entry->cursor.key;
                continue;
            }
        }
        if (r) {
            if (r != entry) {
                Q_ASSERT(entry->reference.key.isValid());
                Q_ASSERT(entry->cursor.key.isValid());
                r->references.insert(entry->cursor);
            }
        } else {
            bool warn = true;
            switch (entry->cursor.key.kind) {
            case CXCursor_InclusionDirective:
                warn = false;
                break;
            default:
                break;
            }
            switch (entry->reference.key.kind) {
            case CXCursor_TemplateTypeParameter:
            case CXCursor_NonTypeTemplateParameter:
                warn = false;
            case CXCursor_ClassDecl:
            case CXCursor_StructDecl:
                if (!entry->reference.key.isDefinition())
                    warn = false;
                break;
            default:
                break;
            }

            // if (warn && entry->cursor.key == entry->reference.key)
            //     warn = false;

            if (warn && !strncmp("operator", entry->cursor.key.symbolName.constData(), 8))
                warn = false;

            // warn = true;
            if (warn) {
                qWarning() << "nowhere to add this reference"
                           << entry->cursor.key << "references" << entry->reference.key;
            }
        }
    }

    // QByteArray entries;
    // QDataStream ds(&entries, QIODevice::WriteOnly);
    // ds << mData->data.size();
    foreach(const RBuildPrivate::DataEntry* entry, mData->data) {
        writeEntry(batch, *entry);
        collectDict(*entry, dict);
        // ds << *entry;
    }

    writeDict(batch, dict);

    QSet<Path> allFiles;
    foreach(const RBuildPrivate::Dependencies &dep, mData->dependencies) {
        // qDebug() << dep.file << ctime(&dep.lastModified);
        writeDependencies(batch, dep.file, dep.arguments,
                          dep.lastModified, dep.dependencies, 0);
    }

    if (!(flags & ExcludePCH)) {
        // batch.Put(" ", leveldb::Slice(entries.constData(), entries.size()));
        const QByteArray pchData = Precompile::pchData();
        if (!pchData.isEmpty())
            batch->Put("pch", leveldb::Slice(pchData.constData(), pchData.size()));
    }

    if (!mSourceDir.isEmpty()) {
        Q_ASSERT(mSourceDir.endsWith('/'));
        if (mSourceDir.isDir()) {
            recurseDir(&allFiles, mSourceDir, mSourceDir.size());
        } else {
            fprintf(stderr, "%s is not a directory\n", mSourceDir.constData());
        }
    }
    writeToBatch(batch, leveldb::Slice("sourceDir"), mSourceDir);
    writeToBatch(batch, leveldb::Slice("files"), allFiles);
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
            CursorKey(cursor).isDefinition() ? " def" : "",
            clang_getCString(filename), line, col);
    clang_disposeString(name);
    clang_disposeString(kind);
    clang_disposeString(filename);
}

static inline void addCursor(const CXCursor& cursor, const CursorKey& key, Cursor* data)
{
    Q_ASSERT(key.isValid());
    if (data->key != key) {
        data->key = key;
        data->parentNames.clear();
        data->containingFunction.clear();
        CXCursor parent = cursor;
        QByteArray containingFunction;
        for (;;) {
            parent = clang_getCursorSemanticParent(parent);
            const CXCursorKind kind = clang_getCursorKind(parent);
            if (clang_isInvalid(kind))
                break;
            CXString str = clang_getCursorDisplayName(parent);
            const char *cstr = clang_getCString(str);
            if (!cstr || !strlen(cstr)) {
                clang_disposeString(str);
                break;
            }
            switch (kind) {
            case CXCursor_CXXMethod:
            case CXCursor_FunctionDecl:
            case CXCursor_Constructor:
            case CXCursor_Destructor:
                containingFunction = cstr;
                break;
            case CXCursor_StructDecl:
            case CXCursor_ClassDecl:
                if (!containingFunction.isEmpty()) {
                    containingFunction.prepend("::");
                    containingFunction.prepend(cstr);
                }
                // fall through
            case CXCursor_Namespace:
                data->parentNames.append(cstr);
                break;
            default:
                break;
            }
            clang_disposeString(str);
        }
        if (!containingFunction.isEmpty())
            data->containingFunction = containingFunction;
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

static bool shouldMergeCursors(const CursorKey& oldcurrent, const CursorKey& newcurrent,
                               const CursorKey& oldref, const CursorKey& newref, bool dodebug)
{
    if (!newcurrent.isValid() || !newref.isValid())
        return false;
#ifdef COLLECTDEBUG
    if (dodebug) {
        qDebug() << "merge?" << oldcurrent << "vs" << newcurrent;
        qDebug() << oldref << "vs" << newref;
        qDebug() << "------";
    }
#else
    Q_UNUSED(dodebug);
#endif
    if (oldref.kind == CXCursor_Constructor && newref.kind == CXCursor_ClassDecl)
        return false;
    if (oldref.kind == CXCursor_ClassDecl && newref.kind == CXCursor_Constructor)
        return true;
    if (oldcurrent.isValid() && oldref.isValid() && oldref.isDefinition()) {
        if ((oldcurrent.kind == CXCursor_CallExpr && (newcurrent.kind == CXCursor_TypeRef || newcurrent.kind == CXCursor_DeclRefExpr))
            || (oldref.locationKey() == oldcurrent.locationKey())) {
            return true;
        }
        return false;
    }
    if (newref.isValid())
        return true;
    return false;
}

static inline bool isSource(const AtomicString &str)
{
    const QByteArray b = str.toByteArray();
    const int dot = b.lastIndexOf('.');
    const int len = b.size() - dot - 1;
    return (dot != -1 && len > 0 && Path::isSource(b.constData() + dot + 1, len));
}

static CXChildVisitResult collectSymbols(CXCursor cursor, CXCursor, CXClientData client_data)
{
    ++symbols;
    const CursorKey key(cursor);
    if (!key.isValid())
        return CXChildVisit_Recurse;
    RBuildPrivate* data = reinterpret_cast<RBuildPrivate*>(client_data);
    ++validSymbols;
    if (!data->restrictFile.isEmpty() && key.fileName != data->restrictFile && !::isSource(key.fileName)) {
        // qDebug() << "ditched" << key << "for" << data->restrictFile;
        return CXChildVisit_Continue;
    }
    ++visitedSymbols;

    RBuildPrivate::DataEntry* entry = 0;
    const QByteArray locationKey = key.locationKey();
    const QHash<QByteArray, RBuildPrivate::DataEntry*>::const_iterator it = data->seen.find(locationKey);
    static const bool verbose = getenv("VERBOSE");
    if (verbose) {
        debugCursor(stderr, cursor);
    }

#ifdef COLLECTDEBUG
    const bool dodebug = (key.fileName.toByteArray().endsWith("main.cpp") && key.line == 10 && key.col == 1);
#else
    const bool dodebug = false;
#endif
    if (it != data->seen.end())
        entry = it.value();
    else
        entry = new RBuildPrivate::DataEntry;

    if (key.kind == CXCursor_InclusionDirective) {
        CursorKey inclusion;
        inclusion.fileName = eatString(clang_getFileName(clang_getIncludedFile(cursor)));
        inclusion.symbolName = removePath(inclusion.fileName.toByteArray());
        inclusion.line = inclusion.col = 1;
        inclusion.off = 0;
        // qDebug() << "found include thing" << inclusion.symbolName << key.toString() << inclusion.toString();
        addCursor(cursor, key, &entry->cursor);
        addCursor(clang_getNullCursor(), inclusion, &entry->reference);
        if (it == data->seen.end()) {
            data->seen[locationKey] = entry;
            data->data.append(entry);
        }
        return CXChildVisit_Continue;
    }

    // VerifyEntry v(entry, cursor);

#ifdef COLLECTDEBUG
    if (dodebug) {
        debugCursor(stdout, cursor);
        fprintf(stdout, "cursor %p\n", entry);
    }
#endif
    const CXCursor definition = key.isDefinition() ? cursor : clang_getCursorDefinition(cursor);
    const CursorKey definitionKey(definition);
    if (!definitionKey.isDefinition()) {
#ifdef COLLECTDEBUG
        if (dodebug)
            printf("no definition:\n");
#endif
        const CXCursor reference = clang_getCursorReferenced(cursor);
        const CursorKey referenceKey(reference);
        if (shouldMergeCursors(entry->cursor.key, key, entry->reference.key, referenceKey, dodebug)) {
            addCursor(cursor, key, &entry->cursor);
            addCursor(reference, referenceKey, &entry->reference);
        } else {
            if (it == data->seen.end()) {
                delete entry;
                entry = 0;
            }
            return CXChildVisit_Recurse;
        }
#ifdef COLLECTDEBUG
        if (dodebug) {
            debugCursor(stdout, reference);
            fprintf(stdout, "ref %p\n", entry);
            qDebug() << entry->reference.key;
        }
#endif
    } else {
#ifdef COLLECTDEBUG
        if (dodebug)
            printf("has definition:\n");
#endif
        if (shouldMergeCursors(entry->cursor.key, key, entry->reference.key, definitionKey, dodebug)) {
            addCursor(cursor, key, &entry->cursor);
            addCursor(definition, definitionKey, &entry->reference);
        } else {
            if (it == data->seen.end()) {
                delete entry;
                entry = 0;
            }
            return CXChildVisit_Recurse;
        }
#ifdef COLLECTDEBUG
        if (dodebug) {
            debugCursor(stdout, definition);
            fprintf(stdout, "def %p\n", entry);
            qDebug() << entry->reference.key;
        }
#endif
    }

    // printf("%s:%d }\n", __FILE__, __LINE__);
    if (it == data->seen.end()) {
        data->seen[locationKey] = entry;
        data->data.append(entry);
    }

    // printf("%s:%d return CXChildVisit_Recurse;\n", __FILE__, __LINE__);
    return CXChildVisit_Recurse;
}

struct InclusionUserData {
    InclusionUserData(QHash<Path, quint64> &deps)
        : dependencies(deps)
    {}
    QList<Path> directIncludes;
    QHash<Path, quint64> &dependencies;
};

static inline void getInclusions(CXFile includedFile,
                                 CXSourceLocation* inclusionStack,
                                 unsigned evilUnsigned,
                                 CXClientData userData)
{
    const int includeLen = evilUnsigned;
    if (includeLen) {
        InclusionUserData *u = reinterpret_cast<InclusionUserData*>(userData);
        CXString str = clang_getFileName(includedFile);
        Path p = Path::resolved(clang_getCString(str));
        u->dependencies[p] = p.lastModified();
        clang_disposeString(str);
        // printf("Included file %s %d\n", eatString(clang_getFileName(includedFile)).constData(), includeLen);
        // qDebug() << includeLen;
        for (int i=0; i<includeLen - 1; ++i) {
            CXFile f;
            clang_getSpellingLocation(inclusionStack[i], &f, 0, 0, 0);
            str = clang_getFileName(f);
            p = Path::resolved(clang_getCString(str));
            if (pchEnabled && i == includeLen - 2)
                u->directIncludes.append(p);
            u->dependencies.insert(p, p.lastModified());
            clang_disposeString(str);
            // printf("    %d %s\n", i, eatString(clang_getFileName(f)).constData());
        }
    }
}

static inline bool diagnose(CXTranslationUnit unit)
{
    if (!unit)
        return false;
    const bool verbose = (getenv("VERBOSE") != 0);
    bool foundError = false;
    const unsigned int numDiags = clang_getNumDiagnostics(unit);
    for (unsigned int i = 0; i < numDiags; ++i) {
        CXDiagnostic diag = clang_getDiagnostic(unit, i);
        const bool error = clang_getDiagnosticSeverity(diag) >= CXDiagnostic_Error;
        foundError = foundError || error;
        if (verbose || error) {
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
        }
        clang_disposeDiagnostic(diag);
    }
    return !foundError;
}

void RBuild::compile(const GccArguments& arguments, bool *usedPch)
{
    Q_ASSERT(arguments.input().size() == 1);
    const Path input = arguments.input().first();
    bool verbose = (getenv("VERBOSE") != 0);

    QList<QByteArray> arglist;
    bool pch = false;
    Precompile *pre = 0;
    // qDebug() << "pchEnabled" << pchEnabled;
    arglist << "-cc1" << "-x" << arguments.languageString() << "-fsyntax-only";

    arglist += arguments.arguments("-I");
    arglist += arguments.arguments("-D");
    arglist += mSysInfo.systemIncludes();

    if (pchEnabled) {
        pre = Precompile::precompiler(arguments);
        Q_ASSERT(pre);
        const QByteArray pchFile = pre->filePath();
        if (!pchFile.isEmpty()) {
            Q_ASSERT(arguments.isCompile());
            pch = true;
            arglist += "-include-pch";
            arglist += pchFile;
        }
    }


    // ### not very efficient
    QVector<const char*> argvector;
    if (verbose)
        fprintf(stderr, "clang ");
    foreach(const QByteArray& arg, arglist) {
        argvector.append(arg.constData());
        if (verbose)
            fprintf(stderr, "%s ", arg.constData());
    }
    if (verbose)
        fprintf(stderr, "%s\n", input.constData());

    CXTranslationUnit unit = 0;
    if (pch) {
        unit = clang_parseTranslationUnit(mIndex, input.constData(),
                                          argvector.constData(), argvector.size(),
                                          0, 0, CXTranslationUnit_DetailedPreprocessingRecord);
        if (!diagnose(unit)) {
            qWarning("Couldn't compile with pch %p, Falling back to no pch", unit);
            clang_disposeTranslationUnit(unit);
            unit = 0;
            argvector.resize(argvector.size() - 2);
            pch = false;
        }
    }
    if (!unit) {
        unit = clang_parseTranslationUnit(mIndex, input.constData(),
                                          argvector.constData(), argvector.size(),
                                          0, 0, CXTranslationUnit_DetailedPreprocessingRecord);
        diagnose(unit);
    }

    if (usedPch)
        *usedPch = pch;

    if (!unit) {
        qWarning() << "Unable to parse unit for" << input << arglist;
        return;
    }

    CXCursor unitCursor = clang_getTranslationUnitCursor(unit);
    if (pchEnabled)
        mData->restrictFile = input;
    clang_visitChildren(unitCursor, collectSymbols, mData);
    if (pchEnabled)
        mData->restrictFile.clear();
    RBuildPrivate::Dependencies deps = { input, arguments, input.lastModified(),
                                         QHash<Path, quint64>() };
    if (usedPch)
        *usedPch = pch;
    if (pch) {
        deps.dependencies = pre->dependencies();
    } else {
        InclusionUserData u(deps.dependencies);
        clang_getInclusions(unit, getInclusions, &u);
    }

    mData->dependencies.append(deps);
    // qDebug() << input << mData->dependencies.last().dependencies.keys();
    clang_disposeTranslationUnit(unit);

    // qDebug() << arguments.raw() << arguments.language();
}

void RBuild::precompileAll()
{
    int i = 0;
    const QList<Precompile*> precompiles = Precompile::precompiles();
    foreach(Precompile *pch, precompiles) {
        if (!pch->isCompiled()) {
            const qint64 before = timer.elapsed();
            CXTranslationUnit unit = pch->precompile(mSysInfo.systemIncludes(), mIndex);
            if (unit) {
                CXCursor unitCursor = clang_getTranslationUnitCursor(unit);
                const int old = mData->data.size();
                symbols = validSymbols = visitedSymbols = 0;
                clang_visitChildren(unitCursor, collectSymbols, mData);
                QHash<Path, quint64> dependencies;
                InclusionUserData u(dependencies);
                clang_getInclusions(unit, getInclusions, &u);
                // qDebug() << dependencies;
                pch->setDependencies(dependencies);
                clang_disposeTranslationUnit(unit);
                const qint64 elapsed = timer.elapsed() - before;
                fprintf(stderr, "parsed pch header (%s) (%d/%d), %d new items (%lld ms) (%d, %d, %d)\n",
                        pch->headerFilePath().constData(), ++i, precompiles.size(),
                        mData->data.size() - old, elapsed,
                        symbols, validSymbols, visitedSymbols);
            }
        }
    }
}
