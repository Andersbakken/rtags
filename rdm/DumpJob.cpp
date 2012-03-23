#include "DumpJob.h"
#include <clang-c/Index.h>
#include <Rdm.h>

DumpJob::DumpJob(const QByteArray& fn, int i)
    : fileName(fn), id(i)
{
}

void DumpJob::run()
{
    leveldb::DB* db = 0;
    leveldb::Options options;
    const QByteArray name = Database::databaseName(Database::Symbol);
    if (name.isEmpty()) {
        emit complete(id, QList<QByteArray>());
        return;
    }

    leveldb::Status status = leveldb::DB::Open(options, name.constData(), &db);
    if (!status.ok()) {
        emit complete(id, QList<QByteArray>());
        return;
    }

    const leveldb::ReadOptions readopts;
    leveldb::Iterator* it = db->NewIterator(readopts);
    it->Seek(fileName.constData());
    QList<QByteArray> out;
    while (it->Valid()) {
        const leveldb::Slice k = it->key();
        if (strncmp(fileName.constData(), k.data(), fileName.size()))
            break;
        Rdm::CursorInfo cursorInfo;

        it->Next();
    }

    delete it;
    emit complete(id, out);
}
