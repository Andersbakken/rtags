#ifndef Path_h
#define Path_h

#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <stdlib.h>
#include <unistd.h>
#include <QByteArray>
#include "Utils.h"
#include <clang-c/Index.h>

class Path : public QByteArray
{
public:
    Path(const Path &other)
        : QByteArray(other)
    {}
    Path(const QByteArray &other)
        : QByteArray(other)
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

    enum Type {
        Invalid,
        File,
        Directory,
        CharacterDevice,
        BlockDevice,
        NamedPipe,
        SymLink,
        Socket,
    };

    inline bool exists() const { return type() != Invalid; }
    inline bool isDir() const { return type() == Directory; }
    inline bool isFile() const { return type() == File; }
    inline bool isAbsolute() const { return (!isEmpty() && at(0) == '/'); }
    bool isResolved() const;
    Path parentDir() const;
    Type type() const;
    bool resolve(const Path &cwd = Path());
    time_t lastModified() const;
    qint64 fileSize() const;
    static Path resolved(const QByteArray &path, bool *ok = 0);
    static Path eatCXString(CXString string) { return eatString(string); }
};
Q_DECLARE_METATYPE(Path);

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
