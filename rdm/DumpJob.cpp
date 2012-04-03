#include "DumpJob.h"
#include <clang-c/Index.h>
#include <Rdm.h>
#include "LevelDB.h"

DumpJob::DumpJob(const QByteArray& fn, int i)
    : fileName(fn), id(i)
{
}

void DumpJob::run()
{
    LevelDB db;
    if (!db.open(Database::Symbol, LevelDB::ReadOnly)) {
        emit complete(id, QList<QByteArray>());
        return;
    }

    const leveldb::ReadOptions readopts;
    leveldb::Iterator* it = db.db()->NewIterator(readopts);
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
        out.append(str.toLocal8Bit());
        it->Next();
    }

    delete it;
    emit complete(id, out);
}
