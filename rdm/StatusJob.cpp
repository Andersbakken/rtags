#include "StatusJob.h"
#include <clang-c/Index.h>
#include <Rdm.h>

StatusJob::StatusJob(int i)
    : id(i)
{
}

void StatusJob::run()
{
    const QList<QByteArray> ret = UnitCache::instance()->status();
    emit complete(id, ret);
}
