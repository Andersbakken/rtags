#ifndef Database_h
#define Database_h

#include "Path.h"
#include "Location.h"
#include <QtCore>

class Connection
{
public:
    Connection() {}
    virtual ~Connection() {}

    virtual QByteArray readData(const QByteArray &key) const = 0;
    virtual void writeData(const QByteArray &key, const QByteArray &value) = 0;
};

typedef QHash<QList<QByteArray>, QSet<Location> > DictionaryHash;

static inline uint qHash(const QList<QByteArray> &scope)
{
    uint ret = 0;
    int idx = 0;
    foreach(const QByteArray &s, scope) {
        ret += (::qHash(s) << idx++);
        // ### is this good?
    }
    return ret;
}

struct Entity {
    Entity() {}
    QByteArray symbolName;
    QList<QByteArray> cursorScope;
    Location definition, super;
    QSet<Location> declarations, references, extraDeclarations, subs;
};

class Database
{
public:
    Database();
    virtual ~Database();

    int count() const; // ### slow

    enum Mode {
        ReadOnly,
        WriteOnly,
        ReadWrite
    };
    static Database* create(const Path &path, Mode mode);

    Mode mode() const { return mMode; }
    Path path() const { return mPath; }
    bool open(const Path &db, Mode mode);
    int close();
    virtual bool isOpened() const = 0;
    Location followLocation(const Location &source) const;
    QSet<Location> findReferences(const Location &location) const;
    QSet<Location> findSymbol(const QByteArray &symbolName) const;
    QList<QByteArray> listSymbols(const QByteArray &filter = QByteArray()) const;
    Location findSuper(const Location &location) const;
    QSet<Location> findSubs(const Location &location) const;
    void writeEntity(const Entity &entity);
    QList<Location> allReferences(const Location &locations) const;
    QSet<Location> allLocations() const; // slow, for tests

    void invalidateEntries(const QSet<Path> &paths);

    Location createLocation(const QByteArray &arg, const Path &cwd = Path());
    enum LocationToStringFlag {
        None = 0x0,
        RelativeToRoot = 0x1
    };
    QByteArray locationToString(const Location &location, unsigned flags = None) const;
    Path path(const Location &location) const { return mFilesByIndex.value(location.file); }

    enum ConnectionType {
        General = 0,
        Dictionary,
        References,
        Targets,
        ExtraDeclarations,
        Super,
        Subs,
        NumConnectionTypes
    };

    class iterator
    {
    public:
        iterator(ConnectionType t)
            : type(t)
        {}

        virtual ~iterator() {}
        virtual QByteArray value() const = 0;
        virtual QByteArray key() const = 0;
        virtual bool seek(const QByteArray &key) = 0;
        virtual bool next() = 0;
        virtual bool isValid() const = 0;
        const ConnectionType type;

        template <typename T> T value(const T &defaultValue = T()) const
        {
            const QByteArray val = value();
            if (val.isEmpty())
                return defaultValue;
            return Database::decodeValue<T>(val);
        }
    };


    template <typename T> T read(const QByteArray &key, const T &defaultValue = T()) const
    {
        return read<T>(General, key, defaultValue);
    }
    template <typename T> void write(const QByteArray &key, const T &t)
    {
        write(General, key, t);
    }

    void remove(const QByteArray &key)
    {
        remove(General, key);
    }

    template <typename T> static QByteArray encodeValue(const T &t)
    {
        QByteArray v;
        {
            QDataStream ds(&v, QIODevice::WriteOnly);
            ds << t;
        }
        return v;
    }

    static QByteArray encodeValue(const QByteArray &ba)
    {
        return ba;
    }

    template <typename T> static T decodeValue(const QByteArray &encoded)
    {
        T t;
        if (!encoded.isEmpty()) {
            QDataStream ds(encoded);
            ds >> t;
        }
        return t;
    }
    static QByteArray decodeValue(const QByteArray &data)
    {
        return data;
    }

    virtual iterator *createIterator(ConnectionType) const = 0;
    Path file(int id) const { return mFilesByIndex.value(id); }
protected:
    virtual bool openDatabase(const Path &db, Mode mode) = 0;
    virtual void closeDatabase() = 0;
    virtual Connection *createConnection(ConnectionType type) const = 0;
private:
    static inline QByteArray encodeKey(const QByteArray &key)
    {
        return key;
    }
    static inline QByteArray encodeKey(const Location &loc)
    {
        if (loc.file) {
            char buf[32];
            const int written = snprintf(buf, 32, "%d:%d:%d:", loc.file, loc.line, loc.column);
            Q_ASSERT(written < 32);
            return QByteArray(buf, written);
        }
        return QByteArray();
    }
    static inline QByteArray encodeKey(int key)
    {
        return QByteArray::number(key);
    }

    template <typename T, typename K> T read(ConnectionType type, const K &k, const T &defaultValue = T()) const
    {
        const QByteArray key = encodeKey(k);
        if (key.isEmpty())
            return T();
        if (!mConnections[type])
            mConnections[type] = createConnection(type);
        Q_ASSERT(mConnections[type]);
        const QByteArray dat = mConnections[type]->readData(key);
        if (dat.isEmpty())
            return defaultValue;
        return decodeValue<T>(dat);
    }

    template <typename T, typename K> void write(ConnectionType type, const K &k, const T &t)
    {
        const QByteArray key = encodeKey(k);
        Q_ASSERT(!key.isEmpty());
        if (!mConnections[type])
            mConnections[type] = createConnection(type);
        Q_ASSERT(mConnections[type]);
        mConnections[type]->writeData(key, encodeValue<T>(t));
    }

    template <typename K> void remove(ConnectionType type, const K &k)
    {
        const QByteArray key = encodeKey(k);
        Q_ASSERT(!key.isEmpty());
        if (!mConnections[type])
            mConnections[type] = createConnection(type);
        Q_ASSERT(mConnections[type]);
        mConnections[type]->writeData(key, QByteArray());
    }

    bool filterLocationSet(Database::iterator *iterator, const QSet<int> &dirty);
    bool filterDictionary(Database::iterator *iterator, const QSet<int> &dirty);

    Path mPath;
    Mode mMode;
    mutable Connection *mConnections[NumConnectionTypes];
    QHash<Path, unsigned> mFilesByName; // ### this is duplicated in RBuildPrivate::filesByName
    QHash<unsigned, Path> mFilesByIndex;

    QHash<QByteArray, DictionaryHash> mDictionary;
    int mRefIdxCounter;
    QSet<int> mRemovedRefs;
    QHash<Location, int> mRefs;

    friend class RBuild;
};

#endif
