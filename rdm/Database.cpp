#include "Database.h"
#include "LevelDB.h"
#include <dirent.h>

Database::Database()
    : mMode(ReadOnly), mRefIdxCounter(0)
{
    memset(mConnections, 0, sizeof(mConnections));
}

Database::~Database()
{
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


bool Database::open(const Path &db, Mode mode)
{
    Q_ASSERT(!isOpened());
    if (mode == WriteOnly)
        removeDirectory(db);
    if (openDatabase(db, mode)) {
        mMode = mode;
        mPath = db;
        mPath.resolve();
        switch (mode) {
        case ReadOnly:
        case ReadWrite: {
            const int version = read<int>("dbVersion", -1);
            if (version != RTags::DatabaseVersion) {
                fprintf(stderr, "Incompatible database version. Expected %d, got %d\n",
                        RTags::DatabaseVersion, version);
                close();
                return false;
            }
            break; }
        case WriteOnly:
            write<int>("dbVersion", RTags::DatabaseVersion);
            break;
        }

        mFilesByName = read<QHash<Path, unsigned> >("filesByName");
        for (QHash<Path, unsigned>::const_iterator it = mFilesByName.begin();
             it != mFilesByName.end(); ++it) {
            mFilesByIndex[it.value()] = it.key();
        }
        return true;
    }
    return false;
}

// true means all locations were filtered out
bool Database::filterLocationSet(Database::iterator *iterator, const QSet<int> &dirty)
{
    QSet<Location> locations = iterator->value<QSet<Location> >();
    QSet<Location>::iterator it = locations.begin();
    bool changed = false;
    // qDebug() << iterator->key() << locations << dirty;
    while (it != locations.end()) {
        if (dirty.contains((*it).file)) {
            it = locations.erase(it);
            changed = true;
        } else {
            ++it;
        }
    }
    if (locations.isEmpty()) {
        remove(iterator->type, iterator->key());
        return true;
    } else if (changed) {
        write(iterator->type, iterator->key(), locations);
    }
    return false;
}

bool Database::filterDictionary(Database::iterator *iterator, const QSet<int> &dirty)
{
    DictionaryHash dict = iterator->value<DictionaryHash>();
    DictionaryHash::iterator it = dict.begin();
    bool changed = false;
    // qDebug() << iterator->key() << locations << dirty;
    while (it != dict.end()) {
        QSet<Location> &locations = it.value();
        QSet<Location>::iterator jj = locations.begin();
        while (jj != locations.end()) {
            if (dirty.contains((*jj).file)) {
                jj = locations.erase(jj);
                changed = true;
            } else {
                ++jj;
            }
        }
        if (locations.isEmpty()) {
            it = dict.erase(it);
        } else {
            ++it;
        }
    }
    if (dict.isEmpty()) {
        remove(Dictionary, iterator->key());
        // qDebug() << "killing" << iterator->key();
        return true;
    } else if (changed) {
        write(Dictionary, iterator->key(), dict);
    }
    return false;
}

void Database::invalidateEntries(const QSet<Path> &paths)
{
    // qDebug() << mFilesByIndex;
    QSet<int> dirtyFileIds;
    foreach(const Path &p, paths) {
        Q_ASSERT(mFilesByName.contains(p));
        dirtyFileIds.insert(mFilesByName.value(p));
    }
    Q_ASSERT(mMode == ReadWrite);
    iterator *it = createIterator(References);
    QHash<int, QSet<QByteArray> > refs;
    {
        if (it->isValid()) {
            do {
                const QByteArray key = it->key();
                if (key.endsWith(':')) {
                    const int id = it->value<int>(-1);
                    if (id < 0) {
                        qWarning("Can't decode index %s", it->value().constData());
                        continue;
                    }
                    refs[id].insert(key);
                } else {
                    if (filterLocationSet(it, dirtyFileIds)) {
                        bool ok;
                        int id = key.toInt(&ok);
                        if (!id) {
                            qWarning("Invalid ref %s", key.constData());
                        } else {
                            mRemovedRefs.insert(id);
                            // qDebug() << "killed refs" << id;
                        }
                    }
                }
            } while (it->next());
        }
        for (QHash<int, QSet<QByteArray> >::const_iterator i = refs.begin(); i != refs.end(); ++i) {
            if (mRemovedRefs.contains(i.key())) {
                foreach(const QByteArray &key, i.value()) {
                    remove(References, key);
                    // qDebug() << "killing refs" << Location::fromKey(key) << i.key();
                }
            } else {
                foreach(const QByteArray &key, i.value()) {
                    const Location loc = Location::fromKey(key);
                    if (!loc.file) {
                        qWarning("Invalid key %s", key.constData());
                        continue;
                    }
                    mRefs[loc] = i.key();
                }
            }
            mRefIdxCounter = qMax(i.key(), mRefIdxCounter);
        }

        delete it;
        it = createIterator(Dictionary);
        if (it->isValid()) {
            do {
                filterDictionary(it, dirtyFileIds);
            } while (it->next());
        }
        delete it;
    }
    {
        QList<QByteArray> removedKeys;
        it = createIterator(Targets);
        if (it->isValid()) {
            do {
                const QByteArray key = it->value();
                if (dirtyFileIds.contains(atoi(key.constData()))) {
                    removedKeys.append(key);
                } else {
                    Location location = it->value<Location>();
                    if (dirtyFileIds.contains(location.file))
                        removedKeys.append(key);
                }
            } while (it->next());
        }
        delete it;
        foreach(const QByteArray &key, removedKeys) {
            remove(Targets, key);
        }
    }
    {
        QList<QByteArray> removedKeys;
        it = createIterator(ExtraDeclarations);
        if (it->isValid()) {
            do {
                const QByteArray key = it->value();
                if (dirtyFileIds.contains(atoi(key.constData()))) {
                    remove(ExtraDeclarations, key);
                } else {
                    filterLocationSet(it, dirtyFileIds);
                }
            } while (it->next());
        }
        delete it;
        foreach(const QByteArray &key, removedKeys) {
            remove(Targets, key);
        }



    }
}

// ### nasty, this returns count() if in debug
int Database::close()
{
    int ret = -1;
    // qDebug() << mDictionary;
    switch (mMode) {
    case ReadWrite:
        for (QHash<QByteArray, DictionaryHash>::const_iterator it = mDictionary.begin();
             it != mDictionary.end(); ++it) {
            DictionaryHash l = read<DictionaryHash>(Dictionary, it.key(), DictionaryHash());
            DictionaryHash old = l;
            const DictionaryHash &dh = it.value();
            for (DictionaryHash::const_iterator hit = dh.begin(); hit != dh.end(); ++hit) {
                l[hit.key()] += hit.value();
            }
            write(Dictionary, it.key(), l);
        }
        break;
    case WriteOnly:
        for (QHash<QByteArray, DictionaryHash>::const_iterator it = mDictionary.begin();
             it != mDictionary.end(); ++it) {
            write(Dictionary, it.key(), it.value());
        }
        break;
    case ReadOnly:
        Q_ASSERT(0);
        break;
    }
#ifdef QT_DEBUG
    ret = count();
#endif
    closeDatabase();

    for (int i=0; i<NumConnectionTypes; ++i) {
        delete mConnections[i];
        mConnections[i] = 0;
    }
    return ret;
}


Location Database::followLocation(const Location &source) const
{
    const Location ret = read<Location>(Targets, source);
    if (!ret.file && read<int>(References, source, -1) != -1)
        return source;
    return ret;
}

Location Database::findSuper(const Location &location) const
{
    Location ret = read<Location>(Super, location);
    if (!ret.file)
        ret = read<Location>(Super, followLocation(location));
    return ret;
}
QSet<Location> Database::findSubs(const Location &location) const
{
    QSet<Location> ret = read<QSet<Location> >(Subs, location);
    if (ret.isEmpty())
        ret = read<QSet<Location> >(Subs, followLocation(location));
    return ret;
}

QSet<Location> Database::findReferences(const Location &source) const
{
    QSet<Location> ret;
    int refId = read<int>(References, source, -1);
    if (refId > 0)
        ret = read<QSet<Location> >(References, refId);
    if (ret.isEmpty()) {
        const Location followed = followLocation(source);
        refId = read<int>(References, followed, -1);
        if (refId > 0) {
            ret.remove(followed); // ### is this right?
            ret = read<QSet<Location> >(References, refId);
        }
    }
    return ret;
}

QSet<Location> Database::findSymbol(const QByteArray &symbolName) const
{
    QSet<Location> ret;
    if (!symbolName.isEmpty()) {
        QList<QByteArray> split;

        int idx = 0;
        int l = 0;
        while ((idx = symbolName.indexOf("::", idx)) != -1) {
            split.append(QByteArray::fromRawData(symbolName.constData() + l, idx - l));
            l = (idx += 2);
        }
        const QByteArray name = (l
                                 ? QByteArray::fromRawData(symbolName.constData() + l, symbolName.size() - l)
                                 : symbolName);
        iterator *it = createIterator(Dictionary);
        if (it->seek(name)) {
            forever {
                const QByteArray key = it->key();
                if (name == key
                    || (!name.contains('(') && key.startsWith(name) && key.at(name.size()) == '(')) {
                    const DictionaryHash &dh = it->value<DictionaryHash>();
                    for (DictionaryHash::const_iterator hit = dh.begin(); hit != dh.end(); ++hit) {
                        bool ok = true;
                        const QList<QByteArray> &scope = hit.key();
                        if (!split.isEmpty()) {
                            const int splitSize = split.size();
                            const int scopeSize = scope.size();
                            if (splitSize <= scopeSize) {
                                const int diff = scopeSize - splitSize;
                                for (int i=splitSize - 1; i>=0; --i) {
                                    if (split.at(i).isEmpty() || split.at(i) != scope.at(i + diff)) {
                                        ok = false;
                                        break;
                                    }
                                }
                            } else {
                                ok = false;
                            }
                        }
                        if (ok) {
                            ret += hit.value();
                        }
                    }
                } else {
                    break;
                }
                if (!it->next())
                    break;
            }
        }
        delete it;
    }
    return ret;
}

enum State {
    Out,
    In,
    InArgs
};

static inline State maybeDict(const QByteArray &key, const QByteArray &filter, State state,
                              int paren, QList<QByteArray> &keys)
{
    Q_ASSERT(!key.isEmpty());
    if (state != In) {
        if (key.startsWith(filter)) {
            if (key.size() == filter.size()) {
                state = InArgs;
            } else if (filter.size() == paren) {
                state = In;
            } else {
                state = Out;
            }
        } else {
            state = Out;
        }
    }
    switch (state) {
    case In:
        if (paren != -1)
            keys.append(key.left(paren));
        keys.append(key);
        break;
    case InArgs:
        keys.append(key);
        break;
    case Out:
        break;
    }
    return state;
}

QList<QByteArray> Database::listSymbols(const QByteArray &filter) const
{
    QList<QByteArray> ret;
    iterator *it = createIterator(Dictionary);
    if (it->isValid()) {
        Q_ASSERT(it);
        do {
            const QByteArray key = it->key();
            int paren = key.lastIndexOf('(');
            State state = maybeDict(key, filter, filter.isEmpty() ? In : Out, paren, ret);
            const DictionaryHash &dh = it->value<DictionaryHash>();
            for (DictionaryHash::const_iterator hit = dh.begin(); hit != dh.end(); ++hit) {
                const QList<QByteArray> &scope = hit.key();
                QByteArray k = key;
                for (int i=scope.size() - 1; i>=0; --i) {
                    k.prepend(scope.at(i) + "::");
                    if (paren != -1)
                        paren += scope.at(i).size() + 2;
                    maybeDict(k, filter, state, paren, ret);
                }
            }
        } while (it->next());
    }
    delete it;
    return ret;
}

Location Database::createLocation(const QByteArray &arg, const Path &cwd)
{
    assert(!arg.isEmpty());
    int colon = arg.lastIndexOf(':');
    if (colon == arg.size() - 1)
        colon = arg.lastIndexOf(':', colon - 1);
    if (colon == -1) {
        return Location();
    }
    const unsigned col = atoi(arg.constData() + colon + 1);
    if (!col) {
        return Location();
    }
    colon = arg.lastIndexOf(':', colon - 1);
    if (colon == -1) {
        return Location();
    }
    const unsigned line = atoi(arg.constData() + colon + 1);
    if (!line) {
        return Location();
    }
    Path file = Path::resolved(arg.left(colon), cwd);
    const int fileId = mFilesByName.value(file, 0);
    if (!fileId) {
        return Location();
    }
    Location loc;
    loc.file = fileId;
    loc.line = line;
    loc.column = col;
    return loc;
}

void Database::writeEntity(const Entity &entity)
{
    if (!entity.definition.file
        && entity.declarations.isEmpty()
        && entity.extraDeclarations.isEmpty()) {
        qDebug() << "why does this happen" << entity.symbolName << entity.cursorScope << entity.definition
                 << entity.declarations << entity.extraDeclarations << entity.references
                 << entity.super << entity.subs;
        return;
    }
    QSet<Location> references = entity.references;
    QSet<Location> extraDeclarations = entity.extraDeclarations;

    int refIdx = 0;
    if (mMode == ReadWrite) {
        if (entity.definition.file)
            refIdx = mRefs.value(entity.definition, 0);
        if (!refIdx) {
            foreach(const Location &l, entity.declarations) {
                refIdx = mRefs.value(l, 0);
                if (refIdx)
                    break;
            }
        }
        if (refIdx)
            references += read<QSet<Location> >(References, refIdx);
    }

    if (!references.isEmpty()) {
        if (!refIdx) {
            if (!mRemovedRefs.isEmpty()) {
                refIdx = *mRemovedRefs.begin();
                mRemovedRefs.erase(mRemovedRefs.begin());
            } else {
                refIdx = ++mRefIdxCounter;
            }
        }
        write(References, refIdx, references);
        Location loc;
        if (entity.definition.file) {
            loc = entity.definition;
        } else if (mMode == ReadWrite) {
            Q_ASSERT(!entity.declarations.isEmpty());
            loc = followLocation(*entity.declarations.begin());
        }
        if (!loc.file && entity.declarations.size() == 1) {
            loc = *entity.declarations.begin();
        }
        if (loc.file) {
            for (QSet<Location>::const_iterator it = references.begin(); it != references.end(); ++it) {
                write(Targets, *it, loc);
            }
        }
    }
    // qDebug() << definition.file << refIdx << declarations.size() << "funky" << symbolName;
    if (entity.definition.file && (refIdx || !entity.declarations.isEmpty()
                                   || !extraDeclarations.isEmpty()
                                   || entity.super.file
                                   || !entity.subs.isEmpty())) {
        const QByteArray key = encodeKey(entity.definition);
        switch (entity.declarations.size()) {
        case 0:
            break;
        case 1:
            write(Targets, key, *entity.declarations.begin());
            break;
        default:
            extraDeclarations += entity.declarations;
            break;
        }
        if (refIdx)
            write(References, key, refIdx);
        if (!extraDeclarations.isEmpty())
            write(ExtraDeclarations, key, extraDeclarations);
        if (entity.super.file)
            write(Super, key, entity.super);
        if (!entity.subs.isEmpty())
            write(Subs, key, entity.subs);
    }

    if (entity.definition.file || refIdx || !entity.subs.isEmpty() || entity.super.file) {
        foreach(const Location &declaration, entity.declarations) {
            const QByteArray key = encodeKey(declaration);
            if (entity.definition.file)
                write(Targets, key, entity.definition);
            if (refIdx)
                write(References, key, refIdx);
            if (!entity.definition.file) {
                if (entity.super.file)
                    write(Super, key, entity.super);
                if (!entity.subs.isEmpty())
                    write(Subs, key, entity.subs);
            }
        }
    }

    if (!entity.symbolName.isEmpty()) {
        QSet<Location> locations = entity.declarations;
        if (entity.definition.file)
            locations.insert(entity.definition);
        QByteArray out;
        mDictionary[entity.symbolName][entity.cursorScope] += locations;
    }
}

QByteArray Database::locationToString(const Location &location, unsigned flags) const
{
    if (location.file) {
        QByteArray ret = mFilesByIndex.value(location.file);
        if (flags & RelativeToRoot) {
            const int slash = mPath.lastIndexOf('/');
            if (slash != -1 && !strncmp(ret.constData(), mPath.constData(), slash + 1))
                ret.remove(0, slash + 1);
        }
        char buf[32];
        snprintf(buf, 32, ":%d:%d:", location.line, location.column);
        ret += buf;
        return ret;
    }
    return QByteArray();
}

Database *Database::create(const Path &path, Mode mode)
{
    LevelDB *l = new LevelDB;
    l->open(path, mode);
    return l;
}

static inline bool lessThan(const Location &l, const Location &r)
{
    if (l.file > r.file)
        return true;
    if (l.file < r.file)
        return false;
    if (l.line > r.line)
        return true;
    if (l.line < r.line)
        return false;
    return l.column > r.column;
}

QList<Location> Database::allReferences(const Location &location) const
{
    Q_ASSERT(location.file);
    QSet<Location> ret;
    ret.insert(location);
    ret += findReferences(location);
    ret += read<QSet<Location> >(ExtraDeclarations, location);
    const Location f = followLocation(location);
    if (f.file) {
        ret += findReferences(f);
        ret.insert(f);
        ret += read<QSet<Location> >(ExtraDeclarations, f);
    }
    ret += read<QSet<Location> >(ExtraDeclarations, location);
    QList<Location> sorted = ret.toList();
    qSort(sorted.begin(), sorted.end(), lessThan);
    return sorted;
}

static Location locationFromKey(const QByteArray &key)
{
    Location loc;
    unsigned *uints[] = { &loc.file, &loc.line, &loc.column };
    const QList<QByteArray> parts = key.split(':'); // ### nasty
    for (int i=0; i<qMin(3, parts.size()); ++i) {
        *uints[i] = parts.at(i).toUInt();
    }
    return loc;
}

QSet<Location> Database::allLocations() const
{
    QSet<Location> ret;
    iterator *it = createIterator(Targets);
    while (it->isValid()) {
        ret += it->value<Location>();
        const Location l = locationFromKey(it->key());
        ret += l;
        foreach(const Location &ll, allReferences(l))
            ret += ll;
        it->next();
    }
    delete it;
    it = createIterator(Dictionary);
    while (it->isValid()) {
        const DictionaryHash &dh = it->value<DictionaryHash>();
        foreach(const QSet<Location> &v, dh) {
            ret += v;
        }
        it->next();
    }
    delete it;
    return ret;
}

int Database::count() const
{
    int count = 0;
    for (int i=0; i<NumConnectionTypes; ++i) {
        iterator *it = createIterator(static_cast<ConnectionType>(i));
        if (it->isValid()) {
            do {
                ++count;
            } while (it->next());
        }
        delete it;
    }
    return count;
}
