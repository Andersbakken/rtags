#include "RecompileJob.h"
#include "UnitCache.h"
#include <Tools.h>

RecompileJob::RecompileJob(const QByteArray& fn, int i)
    : fileName(fn), id(i)
{
}

RecompileJob::~RecompileJob()
{
}

void RecompileJob::run()
{
    CachedUnit locker(fileName, UnitCache::Source | UnitCache::Info);
    UnitCache::Unit* data = locker.unit();
    if (data) {
        // everything is good!
        emit complete(id, QList<QByteArray>());
        return;
    }

    FirstUnitData first;
    first.fileName = fileName;
    visitIncluderFiles(fileName, visitFindFirstUnit, &first, UnitCache::Source | UnitCache::Info);
    if (!first.data)
        warning("recompile: no unit for %s", fileName.constData());

    emit complete(id, QList<QByteArray>());
}
