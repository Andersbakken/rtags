#include "DirtyJob.h"
#include "leveldb/db.h"
#include "Server.h"

void DirtyJob::dirty()
{
    // ### we should probably have a thread or something that stats each file we have in the db and calls dirty if the file is gone
    const leveldb::WriteOptions writeOptions;
    debug() << "DirtyJob::dirty" << mDirty;
    {
        leveldb::DB *db = Server::instance()->db(Server::Symbol);
        RTags::Ptr<leveldb::Iterator> it(db->NewIterator(leveldb::ReadOptions()));
        it->SeekToFirst();
        while (it->Valid()) {
            const leveldb::Slice key = it->key();
            debug() << "looking at" << key.data();
            const int comma = QByteArray::fromRawData(key.data(), key.size()).lastIndexOf(',');
            Q_ASSERT(comma != -1);
            const Path p = QByteArray::fromRawData(key.data(), comma);
            if (mDirty.contains(p)) {
                debug() << "key is dirty. removing" << key.data();
                db->Delete(writeOptions, key);
            } else {
                CursorInfo cursorInfo = Rdm::readValue<CursorInfo>(it);
                if (cursorInfo.dirty(mDirty)) {
                    if (cursorInfo.target.isNull() && cursorInfo.references.isEmpty()) {
                        debug() << "CursorInfo is empty now. removing" << key.data();
                        db->Delete(writeOptions, key);
                    } else {
                        debug() << "CursorInfo is modified. Changing" << key.data();
                        Rdm::writeValue<CursorInfo>(db, key.data(), cursorInfo);
                    }
                }
            }
            it->Next();
        }
    }

    {
        leveldb::DB *db = Server::instance()->db(Server::SymbolName);

        RTags::Ptr<leveldb::Iterator> it(db->NewIterator(leveldb::ReadOptions()));
        it->SeekToFirst();
        while (it->Valid()) {
            QSet<Location> locations = Rdm::readValue<QSet<Location> >(it);
            QSet<Location>::iterator i = locations.begin();
            bool changed = false;
            while (i != locations.end()) {
                if (mDirty.contains((*i).path)) {
                    changed = true;
                    i = locations.erase(i);
                } else {
                    ++i;
                }
            }
            if (changed) {
                if (locations.isEmpty()) {
                    debug() << "No references to" << it->key().data() << "anymore. Removing";
                    db->Delete(writeOptions, it->key());
                } else {
                    debug() << "References to" << it->key().data() << "modified. Changing";
                    Rdm::writeValue<QSet<Location> >(db, it->key().data(), locations);
                }
            }
            it->Next();
        }
    }
}
