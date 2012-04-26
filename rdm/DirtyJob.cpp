#include "DirtyJob.h"
#include "LevelDB.h"

void DirtyJob::dirty()
{
    // ### we should probably have a thread or something that stats each file we have in the db and calls dirty if the file is gone
    const leveldb::WriteOptions writeOptions;
    debug() << "DirtyJob::dirty" << mDirty;
    {
        LevelDB db;
        QByteArray err;
        if (!db.open(Server::Symbol, LevelDB::ReadWrite, &err)) {
            error("Can't open symbol database %s %s\n",
                  Server::databaseDir(Server::Symbol).constData(),
                  err.constData());
        }
        leveldb::Iterator* it = db.db()->NewIterator(leveldb::ReadOptions());
        leveldb::WriteBatch batch;
        bool writeBatch = false;
        it->SeekToFirst();
        while (it->Valid()) {
            const leveldb::Slice key = it->key();
            debug() << "looking at" << key.data();
            const int comma = QByteArray::fromRawData(key.data(), key.size()).lastIndexOf(',');
            Q_ASSERT(comma != -1);
            const Path p = QByteArray::fromRawData(key.data(), comma);
            if (mDirty.contains(p)) {
                debug() << "key is dirty. removing" << key.data();
                batch.Delete(key);
                writeBatch = true;
            } else {
                Rdm::CursorInfo cursorInfo = Rdm::readValue<Rdm::CursorInfo>(it);
                if (cursorInfo.dirty(mDirty)) {
                    writeBatch = true;
                    if (cursorInfo.target.isNull() && cursorInfo.references.isEmpty()) {
                        debug() << "CursorInfo is empty now. removing" << key.data();
                        batch.Delete(key);
                    } else {
                        debug() << "CursorInfo is modified. Changing" << key.data();
                        Rdm::writeValue<Rdm::CursorInfo>(&batch, key.data(), cursorInfo);
                    }
                }
            }
            it->Next();
        }
        delete it;
        if (writeBatch) {
            db.db()->Write(writeOptions, &batch);
        }
    }

    {
        LevelDB db;
        QByteArray err;
        if (!db.open(Server::SymbolName, LevelDB::ReadWrite, &err)) {
            error("Can't open symbol name database %s %s\n",
                  Server::databaseDir(Server::SymbolName).constData(),
                  err.constData());
        }
        leveldb::Iterator* it = db.db()->NewIterator(leveldb::ReadOptions());
        leveldb::WriteBatch batch;
        bool writeBatch = false;
        it->SeekToFirst();
        while (it->Valid()) {
            QSet<RTags::Location> locations = Rdm::readValue<QSet<RTags::Location> >(it);
            QSet<RTags::Location>::iterator i = locations.begin();
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
                writeBatch = true;
                if (locations.isEmpty()) {
                    debug() << "No references to" << it->key().data() << "anymore. Removing";
                    batch.Delete(it->key());
                } else {
                    debug() << "References to" << it->key().data() << "modified. Changing";
                    Rdm::writeValue<QSet<RTags::Location> >(&batch, it->key().data(), locations);
                }
            }
            it->Next();
        }
        delete it;
        if (writeBatch) {
            db.db()->Write(writeOptions, &batch);
        }
    }
}
