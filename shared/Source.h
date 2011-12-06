#ifndef Source_h
#define Source_h

#include <QtCore>
#include "Path.h"

struct Source {
    Path path;
    QList<QByteArray> args;
    quint64 lastModified;
    QHash<Path, quint64> dependencies;
};

static inline QDebug operator<<(QDebug dbg, const Source &s)
{
    dbg << s.path << s.args << s.lastModified << s.dependencies;
    return dbg;
}

static inline QDataStream &operator<<(QDataStream &ds, const Source &s)
{
    ds << s.path << s.args << s.lastModified << s.dependencies;
    return ds;
}

static inline QDataStream &operator>>(QDataStream &ds, Source &s)
{
    ds >> s.path >> s.args >> s.lastModified >> s.dependencies;
    return ds;
}

#endif
