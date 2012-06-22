#include "DumpJob.h"
#include "Database.h"
#include <clang-c/Index.h>
#include <Rdm.h>
#include "Server.h"
#include "ScopedDB.h"
#include "CursorInfo.h"

DumpJob::DumpJob(const ByteArray& fn, int i)
    : Job(i, QueryJobPriority), fileName(fn)
{
}

void DumpJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::Symbol, ScopedDB::Read);
    RTags::Ptr<Iterator> it(db->createIterator());
    it->seek(fileName.constData());
    while (it->isValid() && !isAborted()) {
        const Slice k = it->key();
        if (strncmp(fileName.constData(), k.data(), fileName.size()))
            break;
        const CursorInfo cursorInfo = it->value<CursorInfo>();
        QString str;
        // QDebug dbg(&str);
        // dbg << k << "symbolLength" << cursorInfo.symbolLength
        //     << "target" << cursorInfo.target
        //     << "kind" << Rdm::eatString(clang_getCursorKindSpelling(cursorInfo.kind))
        //     << "references" << cursorInfo.references;
        // write(str.toLocal8Bit());
        it->next();
    }
}
