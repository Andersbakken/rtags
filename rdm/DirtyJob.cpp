#include "DirtyJob.h"
#include "Server.h"

void DirtyJob::dirty()
{
    // ### we should probably have a thread or something that stats each file we have in the db and calls dirty if the file is gone
    debug() << "DirtyJob::dirty" << mDirty;
    {
        ScopedDB db = Server::instance()->db(Server::Symbol, ScopedDB::Write);
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        while (it->isValid()) {
            const Slice key = it->key();
            Q_ASSERT(key.size() == 8);
            const Location loc = Location::fromKey(key.data());
            // debug() << "looking at" << key.data();
            if (mDirty.contains(loc.fileId())) {
                debug() << "key is dirty. removing" << key.data();
                db->remove(key);
            } else {
                CursorInfo cursorInfo = it->value<CursorInfo>();
                if (cursorInfo.dirty(mDirty)) {
                    // ### should we remove the whole cursorInfo if its target and all the references are gone?
                    // if (cursorInfo.target.isNull() && cursorInfo.references.isEmpty()) {
                    //     debug() << "CursorInfo is empty now. removing" << key.data();
                    //     db->remove(key);
                    // } else {
                    debug() << "CursorInfo is modified. Changing" << key.data();
                    db->setValue<CursorInfo>(key, cursorInfo);
                    // }
                }
            }
            it->next();
        }
    }

    {
        ScopedDB db = Server::instance()->db(Server::SymbolName, ScopedDB::Write);

        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        while (it->isValid()) {
            QSet<Location> locations = it->value<QSet<Location> >();
            QSet<Location>::iterator i = locations.begin();
            bool changed = false;
            while (i != locations.end()) {
                if (mDirty.contains((*i).fileId())) {
                    changed = true;
                    i = locations.erase(i);
                } else {
                    ++i;
                }
            }
            if (changed) {
                if (locations.isEmpty()) {
                    debug() << "No references to" << it->key().data() << "anymore. Removing";
                    db->remove(it->key());
                } else {
                    debug() << "References to" << it->key().data() << "modified. Changing";
                    db->setValue<QSet<Location> >(it->key(), locations);
                }
            }
            it->next();
        }
    }
}
