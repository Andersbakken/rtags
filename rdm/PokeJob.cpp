#include "PokeJob.h"
#include "UnitCache.h"
#include <Rdm.h>
#include "Indexer.h"

PokeJob::PokeJob(const QByteArray& fn, int i)
    : fileName(fn), id(i)
{
}

PokeJob::~PokeJob()
{
}

void PokeJob::run()
{
    CachedUnit locker(fileName, UnitCache::Source | UnitCache::Info | UnitCache::AST | UnitCache::Memory);
    UnitCache::Unit* data = locker.unit();
    bool reindex = false;
    if (!data) {
        Rdm::FirstUnitData first;
        first.fileName = fileName;
        Rdm::visitIncluderFiles(fileName, Rdm::visitFindFirstUnit, &first,
                                UnitCache::Source | UnitCache::Info | UnitCache::AST | UnitCache::Memory);
        if (!first.data) {
            warning("recompile: no unit for %s", fileName.constData());
            return;
        }
        reindex = first.data->origin == UnitCache::Source;
        debug() << "Found unit using visitIncluderFiles" << fileName << reindex;
    } else {
        reindex = data->origin == UnitCache::Source;
        debug() << "Found unit" << fileName << reindex;
    }
    if (reindex) {
        Indexer::instance()->index(fileName, QList<QByteArray>(),
                                   UnitCache::AST|UnitCache::Memory|UnitCache::Info|UnitCache::ForceReindex);
    }
}
