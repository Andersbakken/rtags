#include "FollowLocationJob.h"
#include "Rdm.h"

FollowLocationJob::FollowLocationJob(int i, const RTags::Location &loc)
    : id(i), location(loc)
{
}

FollowLocationJob::~FollowLocationJob()
{
}

void FollowLocationJob::run()
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
    Q_ASSERT(db);
    Rdm::CursorInfo cursorInfo = Rdm::findCursorInfo(db, location);
    QList<QByteArray> list;
    if (!cursorInfo.target.isNull())
        list.append(cursorInfo.target.key());
    emit complete(id, list);
}
