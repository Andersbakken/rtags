#include "FindFileJob.h"
#include "RTags.h"
#include "ScopedDB.h"
#include "Server.h"
#include "leveldb/db.h"
#include "CursorInfo.h"

FindFileJob::FindFileJob(const Path &root, const QueryMessage &query)
    : Job(query, 0), mSrcRoot(root)
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
    ScopedDB db = Server::instance()->db(Server::GRFiles, Server::Read, mSrcRoot);
    RTags::Ptr<Iterator> it(db->createIterator());
    it->seekToFirst();
    char buf[PATH_MAX];
    if (mSrcRoot.size() >= static_cast<int>(sizeof(buf)))
        return;
    char *outbuf = queryFlags() & QueryMessage::AbsolutePath ? buf : 0;
    int outbufSize = 0;
    if (outbuf) {
        memcpy(buf, mSrcRoot.constData(), mSrcRoot.size());
        outbufSize = sizeof(buf) - mSrcRoot.size();
        outbuf += mSrcRoot.size();
    }
    enum Mode {
        All,
        RegExp,
        Pattern
    } mode = All;
    if (mRegExp.isValid()) {
        mode = RegExp;
    } else if (!mPattern.isEmpty()) {
        mode = Pattern;
    }
    while (it->isValid()) {
        const ByteArray key = it->key().byteArray();
        bool ok;
        switch (mode) {
        case All:
            ok = true;
            break;
        case RegExp:
            ok = mRegExp.indexIn(key) != -1;
            break;
        case Pattern:
            ok = key.contains(mPattern);
            break;
        }
        if (ok) {
            if (!outbuf) {
                write(key);
            } else if (key.size() + 1 < outbufSize) {
                memcpy(outbuf, key.constData(), key.size() + 1);
                write(ByteArray(buf, mSrcRoot.size() + key.size() + 1));
            }
        }
        it->next();
    }
}
