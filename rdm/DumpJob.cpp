#include "DumpJob.h"
#include <clang-c/Index.h>
#include <Rdm.h>
#include "Server.h"

DumpJob::DumpJob(const QByteArray& fn, int i)
    : Job(i), fileName(fn)
{
}

void DumpJob::run()
{
    leveldb::DB *db = Server::instance()->db(Server::Symbol);
    const leveldb::ReadOptions readopts;
    leveldb::Iterator* it = db->NewIterator(readopts);
    it->Seek(fileName.constData());
    QList<QByteArray> out;
    while (it->Valid()) {
        const leveldb::Slice k = it->key();
        if (strncmp(fileName.constData(), k.data(), fileName.size()))
            break;
        const Rdm::CursorInfo cursorInfo = Rdm::readValue<Rdm::CursorInfo>(it);
        QString str;
        QDebug dbg(&str);
        dbg << QByteArray::fromRawData(k.data(), k.size())
            << "symbolLength" << cursorInfo.symbolLength
            << "target" << cursorInfo.target
            << "kind" << Rdm::eatString(clang_getCursorKindSpelling(cursorInfo.kind))
            << "references" << cursorInfo.references;
        write(str.toLocal8Bit());
        it->Next();
    }

    delete it;
    finish();
}
