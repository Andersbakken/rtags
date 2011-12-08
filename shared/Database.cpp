#include "Database.h"
#include "FileDB.h"
#include "LevelDB.h"
#include <dirent.h>

Database::Database()
    : mMode(ReadOnly), mRefIdxCounter(0)
{}

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
        for (int i=0; i<NumConnectionTypes; ++i) {
            mConnections[i] = createConnection(static_cast<ConnectionType>(i));
            Q_ASSERT(mConnections[i]);
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

struct PendingChange {
    Database::ConnectionType connectionType;
    QByteArray key;
    QSet<Location> locations;
};

static inline bool filterLocations(Database::iterator *iterator, const QSet<int> &dirty,
                                   QList<PendingChange> &changes)
{
    const QByteArray value = iterator->value();
    QDataStream ds(value);
    QSet<Location> locations;
    ds >> locations;
    QSet<Location>::iterator it = locations.begin();
    bool changed = false;
    while (it != locations.end()) {
        if (dirty.contains((*it).file)) {
            it = locations.erase(it);
            changed = true;
        } else {
            ++it;
        }
    }
    if (changed) {
        const PendingChange ch = { iterator->type, iterator->key(), locations };
        changes.append(ch);
    }
    return changed;
}

void Database::invalidateEntries(const QSet<Path> &paths)
{
    QSet<int> dirtyFileIds;
    foreach(const Path &p, paths) {
        Q_ASSERT(mFilesByName.contains(p));
        dirtyFileIds.insert(mFilesByName.value(p));
    }
    Q_ASSERT(mMode == ReadWrite);
    iterator *it = createIterator(References);
    {
        QList<PendingChange> changes;
        if (it->isValid()) {
            do {
                const QByteArray key = it->key();
                if (!key.endsWith(':')) {
                    filterLocations(it, dirtyFileIds, changes);
                    // ### remove empty reference ids?
                }
            } while (it->next());
        }
        delete it;
        it = createIterator(Dictionary);
        if (it->isValid()) {
            do {
                filterLocations(it, dirtyFileIds, changes);
            } while (it->next());
        }
        delete it;
        foreach(const PendingChange &change, changes) {
            if (change.locations.isEmpty()) {
                remove(change.connectionType, change.key);
            } else {
                write(change.connectionType, change.key, change.locations);
            }
        }
    }
    {
        QList<QByteArray> removedKeys;
        it = createIterator(Targets);
        if (it->isValid()) {
            Location location;
            do {
                const QByteArray key = it->value();
                if (dirtyFileIds.contains(atoi(key.constData()))) {
                    removedKeys.append(key);
                } else {
                    const QByteArray value = it->value();
                    QDataStream ds(value);
                    ds >> location;
                    if (dirtyFileIds.contains(location.file)) {
                        removedKeys.append(key);
                    }
                }
            } while (it->next());
        }
        delete it;
        foreach(const QByteArray &key, removedKeys) {
            remove(Targets, key);
        }
    }
}

void Database::close()
{
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

    closeDatabase();

    for (int i=0; i<NumConnectionTypes; ++i) {
        delete mConnections[i];
        mConnections[i] = 0;
    }
}


Location Database::followLocation(const Location &source) const
{
    Location ret;
    if (source.file) {
        char buf[32];
        const int written = snprintf(buf, 32, "%d:%d:%d:", source.file, source.line, source.column);
        Q_ASSERT(written < 32);
        ret = read<Location>(Targets, QByteArray::fromRawData(buf, written), Location());
    }
    return ret;
}

QSet<Location> Database::findReferences(const Location &source) const
{
    QSet<Location> ret;
    if (source.file) {
        char buf[32];
        int written = snprintf(buf, 32, "%d:%d:%d:", source.file, source.line, source.column);
        Q_ASSERT(written < 32);

        const int refId = read<int>(References, QByteArray::fromRawData(buf, written), -1);
        if (refId > 0) {
            written = snprintf(buf, 32, "%d", refId);
            Q_ASSERT(written < 32);
            ret = read<QSet<Location> >(References, QByteArray::fromRawData(buf, written), QSet<Location>());
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
        const int idx = key.indexOf(filter);
        if (idx != -1) {
            if (idx + filter.size() <= paren) {
                state = In;
            } else {
                state = InArgs;
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

QList<QByteArray> Database::symbolNames(const QByteArray &filter) const
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
    assert(!arg.empty());
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

void Database::writeEntity(const QByteArray &symbolName,
                           const QList<QByteArray> &parentNames,
                           const Location &definition,
                           const QSet<Location> &declarations,
                           const QSet<Location> &references)
{
    if (!definition.file && declarations.isEmpty())
        return;

    enum { BufSize = 32 };
    char buf[BufSize];
    int refIdx = 0;
    if (!references.isEmpty()) {
        refIdx = ++mRefIdxCounter;
        const int ret = snprintf(buf, BufSize, "%d", refIdx);
        write(References, QByteArray(buf, ret), references);
        const Location loc = (definition.file ? definition : *declarations.begin());
        for (QSet<Location>::const_iterator it = references.begin();
             it != references.end(); ++it) {
            const Location &l = *it;
            const int ret = snprintf(buf, BufSize, "%d:%d:%d:", l.file, l.line, l.column);
            write(Targets, QByteArray(buf, ret), loc);
        }
        if (definition.file || refIdx) {
            const int ret = snprintf(buf, BufSize, "%d:%d:%d:", definition.file, definition.line, definition.column);
            if (declarations.size() == 1 && definition.file) {
                write(Targets, QByteArray(buf, ret), *declarations.begin());
            }
            if (definition.file)
                write(References, QByteArray(buf, ret), refIdx);

            foreach(const Location &declaration, declarations) {
                const int ret = snprintf(buf, BufSize, "%d:%d:%d:", declaration.file, declaration.line, declaration.column);
                if (definition.file) {
                    write(Targets, QByteArray(buf, ret), definition);
                }
                if (refIdx)
                    write(References, QByteArray(buf, ret), refIdx);
            }
        }
    }

    if (!symbolName.isEmpty()) {
        QSet<Location> locations = declarations;
        if (definition.file)
            locations.insert(definition);
        QByteArray out;
        mDictionary[symbolName][parentNames] += locations;
    }
}

QByteArray Database::locationToString(const Location &location) const
{
    if (location.file) {
        QByteArray ret = mFilesByIndex.value(location.file);
        char buf[32];
        snprintf(buf, 32, ":%d:%d:", location.line, location.column);
        ret += buf;
        return ret;
    }
    return QByteArray();
}

Database *Database::create(const Path &path, Mode mode)
{
    enum Type { leveldb, filedb, error } type = error;
    const QByteArray dbType = qgetenv("RTAGS_DB_TYPE").toLower();
    if (dbType == "leveldb") {
        type = leveldb;
    } else if (dbType == "filedb") {
        type = filedb;
    } else if (dbType.isEmpty()) {
        Path p = path + "/a.idx";
        if (p.exists()) {
            type = filedb;
        } else {
            p = path + "/references";
            if (p.exists()) {
                type = leveldb;
            }
        }
        if (type == error && mode == WriteOnly && dbType.isEmpty())
            type = leveldb;
    }
    switch (type) {
    case leveldb: {
        fprintf(stderr, "Using leveldb\n");
        LevelDB *l = new LevelDB;
        l->open(path, mode);
        return l; }
    case filedb: {
        fprintf(stderr, "Using filedb\n");
        FileDB *f = new FileDB;
        f->open(path, mode);
        return f; }
    case error:
        break;
    }
    qFatal("Unknown db %s", dbType.constData());
    return 0;
}
