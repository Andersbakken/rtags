#ifndef RBuild_p_h
#define RBuild_p_h

#include "CursorKey.h"
#include "AtomicString.h"
#include "GccArguments.h"
#include "RBuild.h"
#include <QList>
#include <QHash>

struct Location
{
    Location() : line(0), column(0) {}
    AtomicString fileName;
    unsigned line, column;
    inline bool operator==(const Location &other) const
    {
        return (line == other.line
                && column == other.column
                && fileName == other.fileName);
    }

    inline bool isNull() const { return fileName.isEmpty(); }
    inline QByteArray key() const
    {
        if (fileName.isEmpty())
            return QByteArray();
        char buf[1024];
        const int ret = snprintf(buf, 1024, "%s:%d:%d", fileName.constData(), line, column);
        return QByteArray(buf, ret);
    }
};

static inline QDebug operator<<(QDebug dbg, const Location &location)
{
    QString str = "Location(";
    if (location.fileName.isEmpty()) {
        str += ")";
    } else {
        str += location.key() + ")";
    }
    dbg << str;
    return dbg;
}

struct Entity {
    Entity() : kind(CXIdxEntity_Unexposed) {}
    AtomicString name;
    CXIdxEntityKind kind;
    Location location, redeclaration;
    QSet<Location> references;
};


static inline uint qHash(const Location &l)
{
    uint h = 0;
    if (!l.fileName.isEmpty()) {
#define HASHCHAR(ch)                            \
        h = (h << 4) + ch;                      \
        h ^= (h & 0xf0000000) >> 23;            \
        h &= 0x0fffffff;                        \
        ++h;

        QByteArray name = l.fileName.toByteArray();
        const char *ch = name.constData();
        Q_ASSERT(ch);
        while (*ch) {
            HASHCHAR(*ch);
            ++ch;
        }
        const unsigned uints[] = { l.line, l.column };
        for (int i=0; i<2; ++i) {
            ch = reinterpret_cast<const char*>(&uints[i]);
            for (int j=0; j<2; ++j) {
                HASHCHAR(*ch);
                ++ch;
            }
        }
    }
#undef HASHCHAR
    return h;
}

struct RBuildPrivate
{
    RBuildPrivate() {}

    QHash<AtomicString, Entity> entities;
    QHash<AtomicString, QList<Location> > references;

    struct Dependencies {
        Path file;
        QList<QByteArray> arguments;
        quint64 lastModified;
        QHash<Path, quint64> dependencies;
    };
    QList<Dependencies> dependencies;
    QMutex entryMutex;
};

class Precompile;
class PrecompileRunnable : public QObject, public QRunnable
{
    Q_OBJECT
public:
    PrecompileRunnable(Precompile *pch,
                       RBuildPrivate *rbp,
                       CXIndex index) // ### is this threadsafe?
        : mPch(pch), mRBP(rbp), mIndex(index)
    {
        setAutoDelete(true);
    }
    virtual void run();
signals:
    void finished(Precompile *pre);
private:
    Precompile *mPch;
    RBuildPrivate *mRBP;
    CXIndex mIndex;
};

#endif
