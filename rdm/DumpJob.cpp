#include "DumpJob.h"
#include "Database.h"
#include <clang-c/Index.h>
#include <Rdm.h>
#include "Server.h"
#include "CursorInfo.h"

DumpJob::DumpJob(const QByteArray& fn, int i)
    : Job(i), fileName(fn)
{
}

void DumpJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::Symbol, ScopedDB::Read);
    RTags::Ptr<Iterator> it(db->createIterator());
    it->seek(fileName.constData());
    QList<QByteArray> out;
    while (it->isValid() && !isAborted()) {
        const Slice k = it->key();
        if (strncmp(fileName.constData(), k.data(), fileName.size()))
            break;
        const CursorInfo cursorInfo = it->value<CursorInfo>();
        QString str;
        QDebug dbg(&str);
        dbg << QByteArray::fromRawData(k.data(), k.size())
            << "symbolLength" << cursorInfo.symbolLength
            << "target" << cursorInfo.target
            << "kind" << Rdm::eatString(clang_getCursorKindSpelling(cursorInfo.kind))
            << "references" << cursorInfo.references;
        write(str.toLocal8Bit());
        it->next();
    }
}
