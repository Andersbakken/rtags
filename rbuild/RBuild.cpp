#include "RBuild.h"
#include "Precompile.h"
#include <RTags.h>
#include <QCoreApplication>
#include <QtAlgorithms>
#include <sstream>
#include <clang-c/Index.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/types.h>
#include <errno.h>
#include <dirent.h>
#include "AtomicString.h"
#include "RBuild_p.h"
#include <FileDB.h>
#include <memory>

using namespace RTags;

static const bool pchEnabled = false; //!getenv("RTAGS_NO_PCH") && false;
static QElapsedTimer timer;
int threadPoolSize = -1;

class CompileRunnable : public QRunnable
{
public:
    CompileRunnable(RBuild *rb, const Path &p, const QList<QByteArray> &a, Precompile *pc)
        : rbuild(rb), path(p), args(a), pch(pc)
    {
        setAutoDelete(true);
    }

    virtual void run()
    {
        const qint64 before = timer.elapsed();
        rbuild->compile(args, path, pch);
        const qint64 elapsed = timer.elapsed();
        fprintf(stderr, "parsed %s, (%lld ms) (%lld)\n", path.constData(),
                elapsed - before, (elapsed - before) / threadPoolSize);
    }
private:
    RBuild *rbuild;
    const Path path;
    const QList<QByteArray> args;
    Precompile *pch;
};

RBuild::RBuild(QObject *parent)
    : QObject(parent), mData(new RBuildPrivate)
{
    mData->index = clang_createIndex(1, 0);
    if (const char *env = getenv("RTAGS_THREAD_COUNT")) {
        const int threads = atoi(env);
        if (threads > 0)
            mData->threadPool.setMaxThreadCount(threads);
    }
    threadPoolSize = mData->threadPool.maxThreadCount();
    RTags::systemIncludes(); // force creation before any threads are spawned
    connect(this, SIGNAL(compileFinished()), this, SLOT(onCompileFinished()));
    timer.start();
}

RBuild::~RBuild()
{
    clang_disposeIndex(mData->index);
    delete mData;
}

void RBuild::setDBPath(const Path &path)
{
    mData->dbPath = path;
}

bool RBuild::buildDB(const Path& makefile, const Path &sourceDir)
{
    if (!makefile.exists()) {
        fprintf(stderr, "%s doesn't exist\n", makefile.constData());
        return false;
    }
    mData->makefile = makefile;
    mData->sourceDir = sourceDir.isEmpty() ? Path(".") : sourceDir;
    if (!mData->sourceDir.isEmpty()) {
        mData->sourceDir.resolve();
        if (!mData->sourceDir.isDir()) {
            fprintf(stderr, "%s is not a directory\n", sourceDir.constData());
            return false;
        }
        if (!mData->sourceDir.endsWith('/'))
            mData->sourceDir.append('/');
    }

    connect(&mData->parser, SIGNAL(fileReady(const GccArguments&)),
            this, SLOT(processFile(const GccArguments&)));
    connect(&mData->parser, SIGNAL(done()), this, SLOT(makefileDone()));
    mData->parser.run(mData->makefile);
    return true;
}

bool RBuild::updateDB()
{
    if (!openDB(Update))
        return false;
    QMap<int, qint64> snapshots;
    snapshots[__LINE__] = timer.elapsed();
    QList<Source> sources = mData->db->read<QList<Source> >("sources");
    mData->filesByName = mData->db->read<QHash<Path, unsigned> >("filesByName");
    snapshots[__LINE__] = timer.elapsed();

    QList<Source*> reparse;
    QSet<Path> dirty;
    const int sourceCount = sources.size();
    for (int i=0; i<sourceCount; ++i) {
        const Source &source = sources.at(i);
        bool dirtySource = (source.path.lastModified() != source.lastModified);
        for (QHash<Path, quint64>::const_iterator it = source.dependencies.constBegin();
             it != source.dependencies.constEnd(); ++it) {
            if (dirty.contains(it.key())) {
                dirtySource = true;
            } else if (it.key().lastModified() != it.value()) {
                dirty.insert(it.key());
                dirtySource = true;
            }
        }
        if (dirtySource) {
            dirty.insert(source.path);
            reparse.append(&sources[i]);
        } else {
            mData->sources.append(source);
        }
    }
    if (reparse.isEmpty()) {
        printf("Nothing has changed (%lld ms)\n", timer.elapsed());
        return true;
    }
    snapshots[__LINE__] = timer.elapsed();

    mData->db->invalidateEntries(dirty);
    snapshots[__LINE__] = timer.elapsed();
    mData->pendingJobs += reparse.size();
    foreach(Source *source, reparse) {
        mData->threadPool.start(new CompileRunnable(this, source->path, source->args, 0));
    }
    QEventLoop loop;
    connect(this, SIGNAL(finishedCompiling()), &loop, SLOT(quit()));
    loop.exec();
    snapshots[__LINE__] = timer.elapsed();

    for (QHash<QByteArray, Entity>::const_iterator it = mData->entities.begin();
         it != mData->entities.end(); ++it) {
        const Entity &entity = it.value();
        // qDebug() << "writing entity" << entity.name
        //          << entity.definition
        //          << entity.declarations
        //          << entity.references;
        mData->db->writeEntity(entity.name, entity.parentNames, entity.definition,
                               entity.declarations, entity.references);
    }
    mData->db->write("sources", mData->sources);
    snapshots[__LINE__] = timer.elapsed();

    closeDB();
    printf("Updated db %lld ms\n", timer.elapsed());
    // qDebug() << snapshots;
    return true;
}

void RBuild::save()
{
    printf("Done parsing, now writing.\n");
    const qint64 beforeWriting = timer.elapsed();

    // Q_ASSERT(filename.endsWith(".rtags.db"));
    if (!openDB(Create)) {
        return;
    }
    writeData();
    closeDB();
    const qint64 elapsed = timer.elapsed();
    fprintf(stderr, "All done. (total/saving %lld/%lld ms)\n", elapsed, elapsed - beforeWriting);
    qApp->quit();
}

void RBuild::compileAll()
{
    mData->pendingJobs += mData->files.size();
    foreach(const GccArguments &args, mData->files) {
        mData->threadPool.start(new CompileRunnable(this, args.input(), args.clangArgs(), 0));
    }
    // for (QHash<Precompile*, QList<GccArguments> >::const_iterator it = mData->filesByPrecompile.begin();
    //      it != mData->filesByPrecompile.end(); ++it) {
    //     Precompile *pre = it.key();
    //     foreach(const GccArguments &args, it.value())
    //         mData->threadPool.start(new CompileRunnable(this, args, pre));
    // }

    mData->files.clear();
}

static inline bool add(const GccArguments &args, QList<GccArguments> &list)
{
    const Path input = args.input();
    QSet<QByteArray> defines;
    foreach(const GccArguments &a, list) {
        if (a.input() == input && args.language() == a.language()) {
            if (defines.isEmpty())
                defines = args.arguments("-D").toSet();
            if (a.arguments("-D").toSet() == defines) {
                return false;
            }
        }
    }
    list.append(args);
    return true;
}

void RBuild::processFile(const GccArguments& arguments)
{
    if (!pchEnabled) {
        if (!add(arguments, mData->files)) {
            // qDebug() << "ditched dupe" << arguments;
        }
        // mData->files.append(arguments);
        // ### could start to compile now
    } else {
        Precompile *precompiler = Precompile::precompiler(arguments);
        Q_ASSERT(precompiler);
        // if (precompiler->isCompiled()) {
        //     compile(arguments, precompiler);
        // } else {
        if (add(arguments, mData->filesByPrecompile[precompiler])) {
            precompiler->collectHeaders(arguments);
        // } else {
        //     qDebug() << "ditched dupe" << arguments;
        }
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

void RBuild::writeData()
{
    mData->db->write("filesByName", mData->filesByName);

    {
        int size = mData->entities.size();
        QHash<QByteArray, Entity>::iterator it = mData->entities.begin();
        while (size--) {
            const Entity &entity = it.value();
            // qDebug() << "writing entity" << entity.name
            //          << entity.definition
            //          << entity.declarations
            //          << entity.references;
            mData->db->writeEntity(entity.name, entity.parentNames, entity.definition,
                                   entity.declarations, entity.references);
            it = mData->entities.erase(it);
        }
    }
    {
        int size = mData->templateEntities.size();
        QHash<Location, TemplateEntity>::iterator it = mData->templateEntities.begin();
        while (size--) {
            const TemplateEntity &entity = it.value();
            // qDebug() << it.key() << entity.references << entity.name;
            mData->db->writeEntity(entity.name, entity.parentNames, it.key(),
                                   QSet<Location>(), entity.references);
            it = mData->templateEntities.erase(it);
        }
    }
    mData->db->write("sources", mData->sources);
    QSet<Path> allFiles;

    if (!mData->sourceDir.isEmpty()) {
        Q_ASSERT(mData->sourceDir.endsWith('/'));
        if (mData->sourceDir.isDir()) {
            recurseDir(&allFiles, mData->sourceDir, mData->sourceDir.size());
        } else {
            fprintf(stderr, "%s is not a directory\n", mData->sourceDir.constData());
        }
    }
    mData->db->write("sourceDir", mData->sourceDir);
    mData->db->write("files", allFiles);
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

static inline Location createLocation(const CXIdxLoc &l, QHash<Path, unsigned> &files)
{
    Location loc;
    CXFile f;
    clang_indexLoc_getFileLocation(l, 0, &f, &loc.line, &loc.column, 0);
    CXString str = clang_getFileName(f);
    const Path fileName = Path::resolved(clang_getCString(str));
    unsigned &file = files[fileName];
    if (!file)
        file = files.size();
    loc.file = file;
    clang_disposeString(str);
    return loc;
}

static inline Location createLocation(const CXCursor &c, QHash<Path, unsigned> &files)
{
    Location loc;
    CXFile f;
    CXSourceLocation sl = clang_getCursorLocation(c);
    clang_getInstantiationLocation(sl, &f, &loc.line, &loc.column, 0);
    CXString str = clang_getFileName(f);
    const Path fileName = Path::resolved(clang_getCString(str));
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

    Entity *e = 0;
    TemplateEntity *te = 0;
    if (decl->entityInfo->templateKind != CXIdxEntity_NonTemplate) {
        te = &p->templateEntities[createLocation(decl->loc, p->filesByName)];
    } else {
        e = &p->entities[decl->entityInfo->USR];
    }
    QByteArray &name = (e ? e->name : te->name);
    CXIdxEntityKind &kind = (e ? e->kind : te->kind);
    QList<QByteArray> &parentNames = (e ? e->parentNames : te->parentNames);
    if (name.isEmpty()) {
        CXString nm = clang_getCursorDisplayName(decl->cursor); // this one gives us args
        name = clang_getCString(nm);
        clang_disposeString(nm);
        kind = decl->entityInfo->kind;
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
                parentNames.prepend(cstr);
                break;
            default:
                break;
            }
            clang_disposeString(str);
        }
    }

    if (e) {
        if (decl->isDefinition) {
            e->definition = createLocation(decl->loc, p->filesByName);
        } else {
            e->declarations.insert(createLocation(decl->loc, p->filesByName));
        }
    }
}

static inline void indexEntityReference(CXClientData userData, const CXIdxEntityRefInfo *ref)
{
    RBuildPrivate *p = reinterpret_cast<RBuildPrivate*>(userData);
    Entity *e = 0;
    TemplateEntity *te = 0;
    QMutexLocker lock(&p->entryMutex);
    if (ref->referencedEntity->templateKind != CXIdxEntity_NonTemplate) {
        te = &p->templateEntities[createLocation(ref->referencedEntity->cursor, p->filesByName)];
    } else {
        e = &p->entities[ref->referencedEntity->USR];
    }
    QSet<Location> &references = (e ? e->references : te->references);
    const Location loc = createLocation(ref->loc, p->filesByName);
    references.insert(loc);
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

    CXIndexAction action = clang_IndexAction_create(mData->index);
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
    Source src = { file, args, file.lastModified(), QHash<Path, quint64>() };
    if (precompile) {
        src.dependencies = precompile->dependencies();
    } else {
        InclusionUserData u(src.dependencies);
        clang_getInclusions(unit, getInclusions, &u);
    }

    QMutexLocker lock(&mData->entryMutex); // ### is this the right place to lock?
    mData->sources.append(src);
    // qDebug() << input << mData->dependencies.last().dependencies.keys();
    clang_disposeTranslationUnit(unit);

    emit compileFinished();
}

void PrecompileRunnable::run()
{
    // const qint64 before = timer.elapsed();
    // CXTranslationUnit unit = mData->pch->precompile(mData->index);
    // if (unit) {
    //     CXCursor unitCursor = clang_getTranslationUnitCursor(unit);
    //     CollectSymbolsUserData userData = {
    //         mData->rBP->entryMutex, mData->rBP->seen, mData->rBP->data, 0
    //     };

    //     clang_visitChildren(unitCursor, collectSymbols, &userData);
    //     QHash<Path, quint64> dependencies;
    //     InclusionUserData u(dependencies);
    //     clang_getInclusions(unit, getInclusions, &u);
    //     // qDebug() << dependencies;
    //     mData->pch->setDependencies(dependencies);
    //     clang_disposeTranslationUnit(unit);
    //     const qint64 elapsed = timer.elapsed() - before;
    //     fprintf(stderr, "parsed pch header (%s) (%lld ms)\n",
    //             mData->pch->headerFilePath().constData(), elapsed);
    // }
    // emit finished(mData->pch);
}

void RBuild::precompileAll()
{
    const QList<Precompile*> precompiles = Precompile::precompiles();
    foreach(Precompile *pch, precompiles) {
        if (!pch->isCompiled()) {
            PrecompileRunnable *runnable = new PrecompileRunnable(pch, mData, mData->index);
            connect(runnable, SIGNAL(finished(Precompile*)), this, SLOT(onPrecompileFinished(Precompile*)));
            mData->threadPool.start(runnable);
        }
    }
}

void RBuild::onCompileFinished()
{
    if (!--mData->pendingJobs) {
        emit finishedCompiling();
    }
}

void RBuild::onPrecompileFinished(Precompile *pch)
{
    Precompile *p = pch->isCompiled() ? pch : 0;
    foreach(const GccArguments &args, mData->filesByPrecompile[pch]) {
        ++mData->pendingJobs;
        mData->threadPool.start(new CompileRunnable(this, args.input(), args.clangArgs(), p));
    }
}

bool RBuild::openDB(Mode mode)
{
    Q_ASSERT(!mData->db);
    mData->db = Database::create(mData->dbPath,
                                 mode == Update ? Database::ReadWrite : Database::WriteOnly);
    return mData->db->isOpened();
}

void RBuild::closeDB()
{
    if (mData->db) {
        mData->db->close();
        delete mData->db;
        mData->db = 0;
    }
}
