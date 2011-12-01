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
#include "RBuild_p.h"
#include <leveldb/write_batch.h>
#include <memory>

using namespace RTags;

QMap<AtomicString, unsigned> *Location::sFiles = 0;

static const bool pchEnabled = false; //!getenv("RTAGS_NO_PCH") && false;
static QElapsedTimer timer;

RBuild::RBuild(QObject *parent)
    : QObject(parent), mData(new RBuildPrivate)
{
    mData->mIndex = clang_createIndex(1, 0);
    if (const char *env = getenv("RTAGS_THREAD_COUNT")) {
        const int threads = atoi(env);
        if (threads > 0)
            mData->mThreadPool.setMaxThreadCount(threads);
    }
    RTags::systemIncludes(); // force creation before any threads are spawned
    connect(this, SIGNAL(compileFinished()), this, SLOT(onCompileFinished()));
    timer.start();
}

RBuild::~RBuild()
{
    clang_disposeIndex(mData->mIndex);
    delete mData;
}

void RBuild::setDBPath(const Path &path)
{
    mData->mDBPath = path;
}

bool RBuild::buildDB(const Path& makefile, const Path &sourceDir)
{
    if (!makefile.exists()) {
        fprintf(stderr, "%s doesn't exist\n", makefile.constData());
        return false;
    }
    mData->mMakefile = makefile;
    mData->mSourceDir = sourceDir;
    if (!mData->mSourceDir.isEmpty()) {
        mData->mSourceDir.resolve();
        if (!mData->mSourceDir.isDir()) {
            fprintf(stderr, "%s is not a directory\n", sourceDir.constData());
            return false;
        }
        if (!mData->mSourceDir.endsWith('/'))
            mData->mSourceDir.append('/');
    }

    connect(&mData->mParser, SIGNAL(fileReady(const GccArguments&)),
            this, SLOT(processFile(const GccArguments&)));
    connect(&mData->mParser, SIGNAL(done()), this, SLOT(makefileDone()));
    mData->mParser.run(mData->mMakefile);
    return true;
}

static inline bool contains(const QHash<Path, GccArguments> &dirty, const AtomicString &fileName)
{
    const Path p = QByteArray::fromRawData(fileName.constData(), fileName.size());
    return dirty.contains(p);
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
    // const qint64 beforeLoad = timer.elapsed();
    // if (!openDB())
    //     return false;
    // QHash<Path, QList<QByteArray> > dirty;
    // int dirtySourceFiles = 0;
    // std::auto_ptr<leveldb::Iterator> it(mData->db->NewIterator(leveldb::ReadOptions()));
    // for (it->Seek("f:"); it->Valid(); it->Next()) {
    //     const leveldb::Slice key = it->key();
    //     if (strncmp(key.data(), "f:", 2))
    //         break;
    //     QList<QByteArray> args;
    //     quint64 lastModified;
    //     QHash<Path, quint64> dependencies;

    //     const leveldb::Slice value = it->value();
    //     const QByteArray data = QByteArray::fromRawData(value.data(), value.size());
    //     QDataStream ds(data);
    //     ds >> args >> lastModified >> dependencies;
    //     const Path file(key.data() + 2, key.size() - 2);
    //     bool recompile = false;
    //     if (lastModified != file.lastModified()) {
    //         recompile = true;
    //         // quint64 lm = dep.file.lastModified();
    //         // qDebug() << dep.file << "has changed" << ctime(&lm) << ctime(&lastModified);
    //     } else {
    //         for (QHash<Path, quint64>::const_iterator it = dependencies.constBegin(); it != dependencies.constEnd(); ++it) {
    //             if (dirty.contains(it.key())) {
    //                 recompile = true;
    //                 break;
    //             } else if (it.key().lastModified() != it.value()) {
    //                 dirty.insert(it.key(), QList<QByteArray>());
    //                 recompile = true;
    //                 break;
    //             }
    //         }
    //     }

    //     if (recompile) {
    //         ++dirtySourceFiles;
    //         dirty.insert(file, args);
    //     }
    //     // qDebug() << file << args.raw() << ctime(&lastModified) << dependencies;
    // }
    // if (!dirtySourceFiles) {
    //     printf("Nothing has changed (%lld ms)\n", timer.elapsed());
    //     return true;
    // }
    // if (!readFromDB(mData->db, "sourceDir", mData->mSourceDir)) {
    //     fprintf(stderr, "Can't read existing data for src dir\n");
    //     return false;
    // }

    // leveldb::WriteBatch batch;
    // for (it->Seek("/"); it->Valid(); it->Next()) {
    //     const leveldb::Slice key = it->key();
    //     Q_ASSERT(!key.empty());
    //     if (key.data()[0] != '/')
    //         break;
    //     const Path p = QByteArray::fromRawData(key.data(), fileNameLength(key.data(), key.size()));
    //     if (dirty.contains(p)) {
    //         batch.Delete(key);
    //         // qDebug() << "ditching" << QByteArray::fromRawData(key.data(), key.size());
    //         continue;
    //     }
    //     const leveldb::Slice value = it->value();
    //     const QByteArray v = QByteArray::fromRawData(value.data(), value.size());
    //     QDataStream ds(v);
    //     QByteArray referredTo;
    //     ds >> referredTo; // read referred to
    //     QSet<Cursor> references;
    //     ds >> references;
    //     bool changed = false;
    //     // qDebug() << "looking at key" << QByteArray::fromRawData(key.data(), key.size()) << references.size();

    //     QSet<Cursor>::iterator sit = references.begin();
    //     while (sit != references.end()) {
    //         const QByteArray fileName = (*sit).key.fileName.toByteArray();
    //         if (dirty.contains(*static_cast<const Path*>(&fileName))) {
    //             // qDebug() << "ditched reference to" << key << "from" << (*sit).key;
    //             sit = references.erase(sit);
    //             changed = true;
    //         } else {
    //             ++sit;
    //         }
    //     }
    //     if (changed) {
    //         // ### could really hang on to this whole thing since we're
    //         // ### quite likely to make an additional change to it
    //         QByteArray out;
    //         {
    //             QDataStream d(&out, QIODevice::WriteOnly);
    //             d << referredTo << references;
    //         }
    //         // qDebug() << "writing to key" << key;
    //         batch.Put(key, leveldb::Slice(out.constData(), out.size()));
    //     }
    // }
    // for (it->Seek("d:"); it->Valid(); it->Next()) {
    //     const leveldb::Slice key = it->key();
    //     Q_ASSERT(!key.empty());
    //     if (strncmp(key.data(), "d:", 2))
    //         break;
    //     QSet<AtomicString> locations = readFromSlice<QSet<AtomicString> >(it->value());
    //     bool foundDirty = false;
    //     QSet<AtomicString>::iterator it = locations.begin();
    //     while (it != locations.end()) {
    //         const AtomicString &k = (*it);
    //         const Path p = QByteArray::fromRawData(k.constData(), fileNameLength(k.constData(), k.size()));
    //         if (dirty.contains(p)) {
    //             foundDirty = true;
    //             // qDebug() << "ditching" << k;
    //             it = locations.erase(it);
    //         } else {
    //             ++it;
    //         }
    //     }
    //     if (foundDirty) {
    //         // qDebug() << "Found dirty for" << QByteArray::fromRawData(key.data(), key.size());
    //         if (locations.isEmpty()) {
    //             batch.Delete(key);
    //         } else {
    //             writeToBatch(&batch, key, locations);
    //         }

    //     }
    // }

    // // return true;
    // // qDebug() << dirty;

    // bool pchDirty = false;
    // {
    //     std::string value;
    //     if (mData->db->Get(leveldb::ReadOptions(), "pch", &value).ok()) {
    //         const QByteArray data = QByteArray::fromRawData(value.c_str(), value.size());
    //         QDataStream ds(data);
    //         int pchCount;
    //         ds >> pchCount;
    //         for (int i=0; i<pchCount; ++i) {
    //             Path pch, header;
    //             GccArguments args;
    //             QHash<Path, quint64> dependencies;
    //             ds >> pch >> header >> args >> dependencies;
    //             if (pch.exists() && header.exists()) {
    //                 bool ok = true;
    //                 for (QHash<Path, quint64>::const_iterator it = dependencies.constBegin();
    //                      it != dependencies.constEnd(); ++it) {
    //                     if (dirty.contains(it.key())) {
    //                         ok = false;
    //                         break;
    //                     } else if (it.key().lastModified() != it.value()) {
    //                         dirty.insert(it.key(), GccArguments());
    //                         ok = false;
    //                         break;
    //                     }
    //                 }
    //                 if (ok) {
    //                     Precompile::create(args, pch, header, dependencies);
    //                 } else {
    //                     pchDirty = false;
    //                 }
    //             }
    //         }
    //     }
    // }

    // printf("Loading data took %lld ms\n", timer.elapsed() - beforeLoad);

    // for (QHash<Path, GccArguments>::const_iterator it = dirty.begin(); it != dirty.end(); ++it) {
    //     const GccArguments &args = it.value();
    //     if (args.isCompile()) {
    //         processFile(args);
    //     }
    // }
    // unsigned writeDataFlags = LookupReferencesFromDatabase;
    // if (pchDirty) {
    //     precompileAll();
    // } else {
    //     writeDataFlags |= ExcludePCH;
    // }
    // QEventLoop loop;
    // connect(this, SIGNAL(finishedCompiling()), &loop, SLOT(quit()));
    // compileAll();
    // loop.exec();

    // // if (count != mData->data.size())
    // //     fprintf(stderr, "Item count changed from %d to %d\n",
    // //             count, mData->data.size());

    // writeData(&batch, writeDataFlags);
    // mData->db->Write(leveldb::WriteOptions(), &batch);
    // closeDB();
    // printf("Updated db %lld ms\n", timer.elapsed());
    return true;
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
    printf("Done parsing, now writing.\n");
    const qint64 beforeWriting = timer.elapsed();

    removeDirectory(mData->mDBPath.constData());
    // Q_ASSERT(filename.endsWith(".rtags.db"));
    if (!openDB()) {
        return;
    }
    leveldb::WriteBatch batch;
    writeData(&batch, 0);
    mData->db->Write(leveldb::WriteOptions(), &batch);
    closeDB();
    const qint64 elapsed = timer.elapsed();
    fprintf(stderr, "All done. (total/saving %lld/%lld ms)\n", elapsed, elapsed - beforeWriting);
    qApp->quit();
}

class CompileRunnable : public QRunnable
{
public:
    CompileRunnable(RBuild *rbuild, const GccArguments &args, Precompile *pch)
        : mRbuild(rbuild), mArgs(args), mPch(pch)
    {
        setAutoDelete(true);
    }

    virtual void run()
    {
        const qint64 before = timer.elapsed();
        mRbuild->compile(mArgs.clangArgs(), mArgs.input(), mPch);
        const qint64 elapsed = timer.elapsed();
        fprintf(stderr, "parsed %s, (%lld ms)\n",
                mArgs.input().constData(), elapsed - before);
    }
private:
    RBuild *mRbuild;
    const GccArguments mArgs;
    Precompile *mPch;
};

void RBuild::compileAll()
{
    mData->mPendingJobs += mData->mFiles.size();
    foreach(const GccArguments &args, mData->mFiles) {
        mData->mThreadPool.start(new CompileRunnable(this, args, 0));
    }
    // for (QHash<Precompile*, QList<GccArguments> >::const_iterator it = mData->mFilesByPrecompile.begin();
    //      it != mData->mFilesByPrecompile.end(); ++it) {
    //     Precompile *pre = it.key();
    //     foreach(const GccArguments &args, it.value())
    //         mData->mThreadPool.start(new CompileRunnable(this, args, pre));
    // }

    mData->mFiles.clear();
}

void RBuild::processFile(const GccArguments& arguments)
{
    if (!pchEnabled) {
        mData->mFiles.append(arguments);
    } else {
        Precompile *precompiler = Precompile::precompiler(arguments);
        Q_ASSERT(precompiler);
        // if (precompiler->isCompiled()) {
        //     compile(arguments, precompiler);
        // } else {
            mData->mFilesByPrecompile[precompiler].append(arguments);
            precompiler->collectHeaders(arguments);
        // }
    }
}

void RBuild::makefileDone()
{
    connect(this, SIGNAL(finishedCompiling()), this, SLOT(save()));
    if (pchEnabled) {
        precompileAll();
    } else {
        compileAll();
    }
}

static inline void writeDependencies(leveldb::WriteBatch* batch, const Path &path, const QList<QByteArray> &args,
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

// static inline void writeEntry(leveldb::WriteBatch* batch, const RBuildPrivate::DataEntry& entry)
// {
//     const CursorKey& key = entry.cursor.key;
//     if (!key.isValid()) {
//         return;
//     }

//     QByteArray k = key.toString();
//     QByteArray v = makeRefValue(entry);
//     if (!v.isEmpty())
//         batch->Put(leveldb::Slice(k.constData(), k.size()), leveldb::Slice(v.constData(), v.size()));
//     // qDebug() << "writing" << k << kindToString(key.kind) << entry.references.size()
//     //          << v.size() << std::string(v.constData(), v.size()).size();
// }

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

static inline void addLocations(const Entity &entity, const QByteArray &key,
                                QHash<AtomicString, QSet<Location> >& dict)
{
    QSet<Location> &locations = dict[key];
    if (entity.definition.file)
        locations.insert(entity.definition);
    foreach(const Location &declaration, entity.declarations)
        locations.insert(declaration);
}

static inline void collectDict(const Entity& entity, QHash<AtomicString, QSet<Location> >& dict)
{
    QByteArray name = entity.name.toByteArray();
    // int colon = name.indexOf('('); // the name we have doesn't include args which kind sorta sucks a little
    // if (colon != -1)
    //     addLocations(entity, AtomicString(name.constData(), colon), dict);
    
    addLocations(entity, name, dict);
    foreach(const AtomicString &cur, entity.parentNames) {
        // const int old = name.size();
        name.prepend("::");
        name.prepend(cur.toByteArray());
        // if (colon != -1) {
        //     colon += (name.size() - old);
        //     addLocations(entity, AtomicString(name.constData(), colon), dict);
        // }

        // qDebug() << "inserting" << name;
        addLocations(entity, name, dict);
    }
}

void RBuild::writeData(leveldb::WriteBatch *batch, unsigned /*flags*/)
{
    Q_ASSERT(batch);
    char buf[512];
    for (QMap<AtomicString, unsigned>::const_iterator it = mData->files.begin(); it != mData->files.end(); ++it) {
        int ret = snprintf(buf, 512, "F:%d", it.value());
        writeToBatch(batch, leveldb::Slice(buf, ret), it.key().toByteArray());
        ret = snprintf(buf, 512, "F:%s", it.key().constData());
        writeToBatch(batch, leveldb::Slice(buf, ret), it.value());
        qDebug() << "files:" << it.value() << it.key();
    }

    QHash<AtomicString, QSet<Location> > dict;
    int refIdxCounter = 0;
    for (QHash<AtomicString, Entity>::const_iterator it = mData->entities.begin(); it != mData->entities.end(); ++it) {
        int refIdx = -1;
        const Entity &entity = it.value();
        if (!entity.references.isEmpty()) {
            refIdx = ++refIdxCounter;
            const int ret = snprintf(buf, 512, "r:%d", refIdx);
            writeToBatch(batch, leveldb::Slice(buf, ret), entity.references);
            const Location loc = (entity.definition.file ? entity.definition : *entity.declarations.begin());
            for (QHash<Location, AtomicString>::const_iterator it = entity.references.begin(); it != entity.references.end(); ++it) {
                QByteArray ref;
                const int ret = snprintf(buf, 512, "%d:%d:%d", it.key().file, it.key().line, it.key().column);
                {
                    QDataStream ds(&ref, QIODevice::WriteOnly);
                    ds << loc << -1;
                }
                qDebug() << "writing entry" << buf << "targets" << loc.key();
                writeToBatch(batch, leveldb::Slice(buf, ret), ref);
            }
        }
        if (entity.definition.file) {
            QByteArray def;
            const int ret = snprintf(buf, 512, "%d:%d:%d", entity.definition.file, entity.definition.line, entity.definition.column);
            {
                QDataStream ds(&def, QIODevice::WriteOnly);
                Location decl;
                if (entity.declarations.size() == 1) {
                    decl = *entity.declarations.begin();
                }
                ds << decl << refIdx;
                qDebug() << "writing entry" << buf << kindToString(entity.kind)
                         << entity.name << entity.parentNames
                         << entity.definition << entity.declarations
                         << entity.references << it.key();
            }
            writeToBatch(batch, leveldb::Slice(buf, ret), def);
        }
        foreach(const Location &declaration, entity.declarations) {
            QByteArray def;
            {
                QDataStream ds(&def, QIODevice::WriteOnly);
                ds << entity.definition << refIdx;
            }
            const int ret = snprintf(buf, 512, "%d:%d:%d", declaration.file, declaration.line, declaration.column);
            writeToBatch(batch, leveldb::Slice(buf, ret), def);
            qDebug() << "writing entry" << buf << entity.name << entity.parentNames
                     << declaration << entity.definition << entity.references
                     << it.key();
        }
        collectDict(entity, dict);
    }
    QHash<AtomicString, QSet<Location> >::const_iterator it = dict.begin();
    const QHash<AtomicString, QSet<Location> >::const_iterator end = dict.end();
    while (it != end) {
        writeToBatch(batch, ("d:" + it.key().toByteArray()), it.value());
        qDebug() << ("d:" + it.key().toByteArray()) << it.value();
        ++it;
    }
    
    //     RBuildPrivate::DataEntry *r = mData->seen.value(refKey);
    //     // if (flags & LookupReferencesFromDatabase) {
    //     //     qDebug() << key << ref << r;
    //     // }
    //     if (!r && (flags & LookupReferencesFromDatabase)) {
    //         std::auto_ptr<leveldb::Iterator> it(db->NewIterator(leveldb::ReadOptions()));
    //         it->Seek(refKey.constData());
    //         if (it->Valid()) {
    //             leveldb::Slice val = it->value();
    //             QByteArray data(val.data(), val.size());
    //             {
    //                 QDataStream ds(&data, QIODevice::ReadWrite);
    //                 QByteArray mapsTo;
    //                 ds >> mapsTo;
    //                 const int pos = ds.device()->pos();
    //                 QSet<Cursor> refs;
    //                 ds >> refs;
    //                 refs.insert(entry->cursor);
    //                 ds.device()->seek(pos);
    //                 ds << refs;
    //             }
    //             batch->Put(it->key(), leveldb::Slice(data.constData(), data.size()));
    //             // qDebug() << "successfully looked up" << refKey << "and added ref" << entry->cursor.key;
    //             continue;
    //         }
    //     }
    //     if (r) {
    //         if (r != entry) {
    //             Q_ASSERT(entry->reference.key.isValid());
    //             Q_ASSERT(entry->cursor.key.isValid());
    //             r->references.insert(entry->cursor);
    //         }
    //     } else {
    //         bool warn = true;
    //         switch (entry->cursor.key.kind) {
    //         case CXCursor_InclusionDirective:
    //             warn = false;
    //             break;
    //         default:
    //             break;
    //         }
    //         switch (entry->reference.key.kind) {
    //         case CXCursor_TemplateTypeParameter:
    //         case CXCursor_NonTypeTemplateParameter:
    //             warn = false;
    //         case CXCursor_ClassDecl:
    //         case CXCursor_StructDecl:
    //             if (!entry->reference.key.isDefinition())
    //                 warn = false;
    //             break;
    //         default:
    //             break;
    //         }

    //         // if (warn && entry->cursor.key == entry->reference.key)
    //         //     warn = false;

    //         if (warn && !strncmp("operator", entry->cursor.key.symbolName.constData(), 8))
    //             warn = false;

    //         // warn = true;
    //         if (warn) {
    //             qWarning() << "nowhere to add this reference"
    //                        << entry->cursor.key << "references" << entry->reference.key;
    //         }
    //     }
    // }

    // // QByteArray entries;
    // // QDataStream ds(&entries, QIODevice::WriteOnly);
    // // ds << mData->data.size();
    // foreach(const RBuildPrivate::DataEntry* entry, mData->data) {
    //     writeEntry(batch, *entry);
    //     collectDict(*entry, dict);
    //     // ds << *entry;
    // }

    // writeDict(batch, dict);

    QSet<Path> allFiles;
    foreach(const RBuildPrivate::Dependencies &dep, mData->dependencies) {
        // qDebug() << dep.file << ctime(&dep.lastModified);
        writeDependencies(batch, dep.file, dep.arguments,
                          dep.lastModified, dep.dependencies, 0);
    }

    // if (!(flags & ExcludePCH)) {
    //     // batch.Put(" ", leveldb::Slice(entries.constData(), entries.size()));
    //     const QByteArray pchData = Precompile::pchData();
    //     if (!pchData.isEmpty())
    //         batch->Put("pch", leveldb::Slice(pchData.constData(), pchData.size()));
    // }

    if (!mData->mSourceDir.isEmpty()) {
        Q_ASSERT(mData->mSourceDir.endsWith('/'));
        if (mData->mSourceDir.isDir()) {
            recurseDir(&allFiles, mData->mSourceDir, mData->mSourceDir.size());
        } else {
            fprintf(stderr, "%s is not a directory\n", mData->mSourceDir.constData());
        }
    }
    writeToBatch(batch, leveldb::Slice("sourceDir"), mData->mSourceDir);
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
    fprintf(out, "cursor name %s, kind %s, loc %s:%u:%u\n",
            clang_getCString(name), clang_getCString(kind),
            clang_getCString(filename), line, col);
    clang_disposeString(name);
    clang_disposeString(kind);
    clang_disposeString(filename);
}

// #define COLLECTDEBUG

static inline bool isSource(const AtomicString &str)
{
    const QByteArray b = str.toByteArray();
    const int dot = b.lastIndexOf('.');
    const int len = b.size() - dot - 1;
    return (dot != -1 && len > 0 && Path::isSource(b.constData() + dot + 1, len));
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

static inline Location createLocation(const CXIdxLoc &l, QMap<AtomicString, unsigned> &files)
{
    Location loc;
    CXFile f;
    clang_indexLoc_getFileLocation(l, 0, &f, &loc.line, &loc.column, 0);
    CXString str = clang_getFileName(f);
    const AtomicString fileName = Path::resolved(clang_getCString(str));
    unsigned &file = files[fileName];
    if (!file)
        file = files.size();
    loc.file = file;
    clang_disposeString(str);
    return loc;
}

static inline void indexDeclaration(CXClientData userData, const CXIdxDeclInfo *decl)
{
    RBuildPrivate *p = reinterpret_cast<RBuildPrivate*>(userData);
    QMutexLocker lock(&p->entryMutex); // ### is this the right place to lock?
    Entity &e = p->entities[decl->entityInfo->USR];
    if (e.name.isEmpty()) {
        // if (decl->isContainer && isValidKind(clang_getCursorKind(decl->container->cursor)))
        //     qDebug() << decl->container->cursor << createLocation(decl->loc);
        e.name = decl->entityInfo->name;
        e.kind = decl->entityInfo->kind;
        CXCursor parent = decl->cursor;
        forever {
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
            case CXCursor_StructDecl:
            case CXCursor_ClassDecl:
            case CXCursor_Namespace:
                e.parentNames.append(cstr);
                break;
            default:
                break;
            }
            clang_disposeString(str);
        }
    }

    if (decl->isDefinition) {
        // qDebug() << "getting definition"
        //          << eatString(clang_getCursorUSR(decl->cursor));
        e.definition = createLocation(decl->loc, p->files);
    } else {
        e.declarations.insert(createLocation(decl->loc, p->files));
    }
    // } else {
    //     qDebug() << "getting something again here" << decl->isRedeclaration
    //              << decl->entityInfo->name
    //              << kindToString(decl->entityInfo->kind)
    //              << createLocation(decl->loc);
}

static AtomicString findContainingFunction(const CXIdxEntityRefInfo *ref)
{
    CXCursor parent = ref->cursor;
    QByteArray containingFunction;
    forever {
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
            break;
        default:
            break;
        }
        clang_disposeString(str);
    }
    return containingFunction;
}

static inline void indexEntityReference(CXClientData userData, const CXIdxEntityRefInfo *ref)
{
    RBuildPrivate *p = reinterpret_cast<RBuildPrivate*>(userData);
    const AtomicString key(ref->referencedEntity->USR);
    const Location loc = createLocation(ref->loc, p->files);
    // qDebug() << loc << kindToString(clang_getCursorKind(ref->cursor))
    //          << (ref->parentEntity ? ref->parentEntity->name : "no parent")
    //          << ref->container->cursor;
    QMutexLocker lock(&p->entryMutex); // ### is this the right place to lock?
    if (!p->entities.contains(key)) {
        static QSet<Location> warned;
        if (!warned.contains(loc)) {
            qDebug() << "couldn't find" << loc << key
                     << ref->referencedEntity->cursor
                     << eatString(clang_getCursorUSR(ref->referencedEntity->cursor));
            warned.insert(loc);
        }
        return;
    }
    Entity &e = p->entities[key];
    if (e.name.isEmpty()) {
        qDebug() << "couldn't find" << loc << key;
    }
    Q_ASSERT(!e.name.isEmpty() &&
             "This needs to be fixed. In case of PCH it needs to "
             "potentially modify references to both the declaration and the definition");
    e.references.insert(loc, findContainingFunction(ref));
}

template <typename T>
QDebug operator<<(QDebug dbg, const QVarLengthArray<T> &arr)
{
    dbg.nospace() << "QVarLengthArray(";
    for (int i=0; i<arr.size(); ++i) {
        if (i > 0)
            dbg.nospace() << ", ";
        dbg.nospace() << arr.at(i);
    }
    dbg.nospace() << ")";
    return dbg.space();
}

void RBuild::compile(const QList<QByteArray> &args, const Path &file, Precompile *precompile)
{
    QVarLengthArray<const char *, 64> clangArgs(args.size() + (file.isEmpty() ? 0 : 2));
    int argCount = 0;
    foreach(const QByteArray& arg, args) {
        clangArgs[argCount++] = arg.constData();
    }
    if (precompile) {
        Q_ASSERT(precompile->isCompiled());
        clangArgs[argCount++] = "-include-pch";
        clangArgs[argCount++] = precompile->filePath().constData();
    }

    IndexerCallbacks cb;
    memset(&cb, 0, sizeof(IndexerCallbacks));
    cb.indexDeclaration = indexDeclaration;
    cb.indexEntityReference = indexEntityReference;

    CXIndexAction action = clang_IndexAction_create(mData->mIndex);
    CXTranslationUnit unit = 0;
    // fprintf(stderr, "clang ");
    // for (int i=0; i<argCount; ++i) {
    //     fprintf(stderr, "%s ", clangArgs[i]);
    // }
    // fprintf(stderr, "%s\n", file.constData());
    

    if (precompile && clang_indexSourceFile(action, mData, &cb, sizeof(IndexerCallbacks),
                                            CXIndexOpt_None, file.constData(), clangArgs.constData(),
                                            argCount, 0, 0, &unit,
                                            clang_defaultEditingTranslationUnitOptions())) {
        qWarning("Couldn't compile %s with pch %p, Falling back to no pch", file.constData(), unit);
        // fprintf(stderr, "clang ");
        // foreach(const QByteArray& arg, arglist) {
        //     fprintf(stderr, "%s ", arg.constData());
        // }
        // fprintf(stderr, "%s\n", input.constData());

        if (unit)
            clang_disposeTranslationUnit(unit);
        unit = 0;
        argCount -= 2;
        precompile = 0;
    }
    if (!unit && clang_indexSourceFile(action, mData, &cb, sizeof(IndexerCallbacks),
                                       CXIndexOpt_None, file.constData(),
                                       clangArgs.constData(), argCount,
                                       0, 0, &unit, clang_defaultEditingTranslationUnitOptions())) {
        if (unit)
            clang_disposeTranslationUnit(unit);
        unit = 0;
    }

    if (!unit) {
        qWarning() << "Unable to parse unit for" << file; // << clangArgs;
        return;
    }
    RBuildPrivate::Dependencies deps = { file, args, file.lastModified(),
                                         QHash<Path, quint64>() };
    if (precompile) {
        deps.dependencies = precompile->dependencies();
    } else {
        InclusionUserData u(deps.dependencies);
        clang_getInclusions(unit, getInclusions, &u);
    }

    QMutexLocker lock(&mData->entryMutex); // ### is this the right place to lock?
    mData->dependencies.append(deps);
    // qDebug() << input << mData->dependencies.last().dependencies.keys();
    clang_disposeTranslationUnit(unit);

    emit compileFinished();

}

// void RBuild::compile(const GccArguments& arguments, Precompile *pre, bool *usedPch)
// {
//     const Path input = arguments.input();
//     bool verbose = (getenv("VERBOSE") != 0);

//     QList<QByteArray> arglist;
//     bool pch = false;
//     // qDebug() << "pchEnabled" << pchEnabled;
//     arglist << "-cc1" << "-x" << arguments.languageString() << "-fsyntax-only";

//     arglist += arguments.arguments("-I");
//     arglist += arguments.arguments("-D");
//     arglist += RTags::systemIncludes();

//     Q_ASSERT(pchEnabled || !pre);
//     Q_ASSERT(!pre || pre->isCompiled());
//     Q_ASSERT(pre);
//     if (pre) {
//         const QByteArray pchFile = pre->filePath();
//         Q_ASSERT(!pchFile.isEmpty());
//         pch = true;
//         arglist += "-include-pch";
//         arglist += pchFile;
//     }


//     // ### not very efficient
//     QVector<const char*> argvector;
//     if (verbose)
//         fprintf(stderr, "clang ");
//     foreach(const QByteArray& arg, arglist) {
//         argvector.append(arg.constData());
//         if (verbose)
//             fprintf(stderr, "%s ", arg.constData());
//     }
//     if (verbose)
//         fprintf(stderr, "%s\n", input.constData());

//     IndexerCallbacks cb;
//     memset(&cb, 0, sizeof(IndexerCallbacks));
//     cb.indexDeclaration = indexDeclaration;
//     cb.indexEntityReference = indexEntityReference;

//     CXIndexAction action = clang_IndexAction_create(mData->mIndex);
//     CXTranslationUnit unit = 0;
//     if (pch && clang_indexSourceFile(action, 0, &cb, sizeof(IndexerCallbacks),
//                                      CXIndexOpt_None, input.constData(), argvector.constData(), argvector.size(),
//                                      0, 0, &unit, clang_defaultEditingTranslationUnitOptions())) {
//         qWarning("Couldn't compile with pch %p, Falling back to no pch", unit);
//         // fprintf(stderr, "clang ");
//         // foreach(const QByteArray& arg, arglist) {
//         //     fprintf(stderr, "%s ", arg.constData());
//         // }
//         // fprintf(stderr, "%s\n", input.constData());

//         if (unit)
//             clang_disposeTranslationUnit(unit);
//         unit = 0;
//         argvector.resize(argvector.size() - 2);
//         pch = false;
//     }
//     if (!unit && clang_indexSourceFile(action, 0, &cb, sizeof(IndexerCallbacks),
//                                        CXIndexOpt_None, input.constData(), argvector.constData(), argvector.size(),
//                                        0, 0, &unit, clang_defaultEditingTranslationUnitOptions())) {
//         if (unit)
//             clang_disposeTranslationUnit(unit);
//         unit = 0;
//     }

//     if (usedPch)
//         *usedPch = pch;

//     if (!unit) {
//         qWarning() << "Unable to parse unit for" << input << arglist;
//         return;
//     }


//     RBuildPrivate::Dependencies deps = { input, arguments, input.lastModified(),
//                                          QHash<Path, quint64>() };
//     if (usedPch)
//         *usedPch = pch;
//     if (pch) {
//         deps.dependencies = pre->dependencies();
//     } else {
//         InclusionUserData u(deps.dependencies);
//         clang_getInclusions(unit, getInclusions, &u);
//     }

//     mData->dependencies.append(deps);
//     // qDebug() << input << mData->dependencies.last().dependencies.keys();
//     clang_disposeTranslationUnit(unit);

//     emit compileFinished();
//     // qDebug() << arguments.raw() << arguments.language();
// }

void PrecompileRunnable::run()
{
    // const qint64 before = timer.elapsed();
    // CXTranslationUnit unit = mData->mPch->precompile(mData->mIndex);
    // if (unit) {
    //     CXCursor unitCursor = clang_getTranslationUnitCursor(unit);
    //     CollectSymbolsUserData userData = {
    //         mData->mRBP->entryMutex, mData->mRBP->seen, mData->mRBP->data, 0
    //     };

    //     clang_visitChildren(unitCursor, collectSymbols, &userData);
    //     QHash<Path, quint64> dependencies;
    //     InclusionUserData u(dependencies);
    //     clang_getInclusions(unit, getInclusions, &u);
    //     // qDebug() << dependencies;
    //     mData->mPch->setDependencies(dependencies);
    //     clang_disposeTranslationUnit(unit);
    //     const qint64 elapsed = timer.elapsed() - before;
    //     fprintf(stderr, "parsed pch header (%s) (%lld ms)\n",
    //             mData->mPch->headerFilePath().constData(), elapsed);
    // }
    // emit finished(mData->mPch);
}

void RBuild::precompileAll()
{
    const QList<Precompile*> precompiles = Precompile::precompiles();
    foreach(Precompile *pch, precompiles) {
        if (!pch->isCompiled()) {
            PrecompileRunnable *runnable = new PrecompileRunnable(pch, mData, mData->mIndex);
            connect(runnable, SIGNAL(finished(Precompile*)), this, SLOT(onPrecompileFinished(Precompile*)));
            mData->mThreadPool.start(runnable);
        }
    }
}

void RBuild::onCompileFinished()
{
    if (!--mData->mPendingJobs) {
        emit finishedCompiling();
    }
}

void RBuild::onPrecompileFinished(Precompile *pch)
{
    Precompile *p = pch->isCompiled() ? pch : 0;
    foreach(const GccArguments &args, mData->mFilesByPrecompile[pch]) {
        ++mData->mPendingJobs;
        mData->mThreadPool.start(new CompileRunnable(this, args, p));
    }
}

bool RBuild::openDB()
{
    Q_ASSERT(!mData->db);
    leveldb::Options dbOptions;
    dbOptions.create_if_missing = true;
    if (!leveldb::DB::Open(dbOptions, mData->mDBPath.constData(), &mData->db).ok()) {
        fprintf(stderr, "Can't open db [%s]\n", mData->mDBPath.constData());
        return false;
    }
    return true;
}

void RBuild::closeDB()
{
    delete mData->db;
    mData->db = 0;
}
