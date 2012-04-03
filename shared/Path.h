#ifndef Path_h
#define Path_h

#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <stdlib.h>
#include <unistd.h>
#include <qbytearray.h>
#include <QMetaObject>

class Path : public QByteArray
{
public:
    Path(const Path &other)
        : QByteArray(other)
    {}
    Path(const QByteArray &other)
        : QByteArray(other)
    {}
    Path(const char *path)
        : QByteArray(path)
    {}
    Path(const char *path, int size)
        : QByteArray(path, size)
    {}
    Path() {}
    Path &operator=(const Path &other)
    {
        QByteArray::operator=(other);
        return *this;
    }
    Path &operator=(const QByteArray &other)
    {
        QByteArray::operator=(other);
        return *this;
    }

    Path &operator=(const char *path)
    {
        QByteArray::operator=(path);
        return *this;
    }

    enum Type {
        Invalid,
        File,
        Directory,
        CharacterDevice,
        BlockDevice,
        NamedPipe,
        SymLink,
        Socket
    };

    inline bool exists() const { return type() != Invalid; }
    inline bool isDir() const { return type() == Directory; }
    inline bool isFile() const { return type() == File; }
    inline bool isAbsolute() const { return (!isEmpty() && at(0) == '/'); }
    const char *fileName() const;
    const char *extension() const;
    bool isSource() const;
    static bool isSource(const char *extension, int len);
    bool isHeader() const;
    bool isResolved() const;
    bool isCanonical() const;
    Path parentDir() const;
    Type type() const;
    bool resolve(const Path &cwd = Path());
    int canonicalize();
    quint64 lastModified() const; // returns time_t
    int64_t fileSize() const;
    static Path resolved(const QByteArray &path, const Path &cwd = Path(), bool *ok = 0);
    static Path canonicalized(const QByteArray &path);
};

static inline QDataStream &operator<<(QDataStream &ds, const Path &path)
{
    ds << QByteArray::fromRawData(path.constData(), path.size());
    return ds;
}

static inline QDataStream &operator>>(QDataStream &ds, Path &path)
{
    QByteArray in;
    ds >> in;
    path = in;
    return ds;
}




#endif
