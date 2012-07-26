#include "FindFileJob.h"
#include "RTags.h"
#include "ScopedDB.h"
#include "Server.h"
#include "leveldb/db.h"
#include "CursorInfo.h"

FindFileJob::FindFileJob(const QueryMessage &query)
    : Job(query, 0)
{
    const ByteArray q = query.query();
    if (!q.isEmpty()) {
        if (query.flags() & QueryMessage::MatchRegExp) {
            mRegExp = q;
        } else {
            mPattern = q;
        }
    }
}

void FindFileJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::Files, Server::Read);
    RTags::Ptr<Iterator> it(db->createIterator());
    it->seekToFirst();
    if (mRegExp.isValid()) {
        while (it->isValid()) {
            const ByteArray key = it->key().byteArray();
            if (mRegExp.indexIn(key) != -1)
                write(key);
            it->next();
        }
    } else if (!mPattern.isEmpty()) {
        while (it->isValid()) {
            const ByteArray key = it->key().byteArray();
            if (key.contains(mPattern))
                write(key);
            it->next();
        }
    } else {
        while (it->isValid()) {
            write(it->key().byteArray());
            it->next();
        }
    }
}
