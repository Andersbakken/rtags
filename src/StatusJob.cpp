#include "StatusJob.h"
#include "CursorInfo.h"
#include "Database.h"
#include "Indexer.h"
#include "RTags.h"
#include "Server.h"
#include "FileInformation.h"
#include <clang-c/Index.h>

const char *StatusJob::delimiter = "*********************************";
StatusJob::StatusJob(const QueryMessage &q, std::tr1::shared_ptr<Indexer> indexer)
    : Job(q, WriteUnfiltered), query(q.query()), mIndexer(indexer)
{
}

void StatusJob::execute()
{
    bool matched = false;
    Server *server = Server::instance();
    if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "general")) {
        matched = true;
        ScopedDB db = server->db(Server::General, ReadWriteLock::Read);
        write(delimiter);
        write(server->databaseDir(Server::General));
        write("    version: " + ByteArray::number(db->value<int>("version")));

        const Map<Path, MakefileInformation> makefiles = db->value<Map<Path, MakefileInformation> >("makefiles");
        for (Map<Path, MakefileInformation>::const_iterator it = makefiles.begin(); it != makefiles.end(); ++it) {
            ByteArray out = "    " + it->first;
            out += " last touched: " + RTags::timeToString(it->second.lastTouched);
            if (!it->second.makefileArgs.isEmpty())
                out += " args: " + ByteArray::join(it->second.makefileArgs, " ");
            if (!it->second.extraFlags.isEmpty())
                out += " extra flags: " + ByteArray::join(it->second.extraFlags, " ");
            write(out);
        }
    }

    if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "fileids")) {
        matched = true;
        ScopedDB db = server->db(Server::FileIds, ReadWriteLock::Read);
        write(delimiter);
        write(server->databaseDir(Server::FileIds));
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        char buf[1024];
        while (it->isValid()) {
            snprintf(buf, 1024, "  %u: %s", it->value<uint32_t>(), it->key().byteArray().constData());
            write(buf);
            it->next();
        }
    }

    const char *alternatives = "general|fileids|dependencies|fileinfos|symbols|symbolnames|pch|visitedfiles";
    if (!mIndexer) {
        if (!matched) {
            write(alternatives);
        }
        return;
    }

    if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "dependencies")) {
        matched = true;
        ScopedDB db = server->db(Server::Dependency, ReadWriteLock::Read);
        write(delimiter);
        write(server->databaseDir(Server::Dependency));
        RTags::Ptr<Iterator> it(db->createIterator());
        Map<uint32_t, Set<uint32_t> > depsReversed;
        it->seekToFirst();
        char buf[1024];
        while (it->isValid()) {
            if (isAborted())
                return;
            const uint32_t key = *reinterpret_cast<const uint32_t*>(it->key().data());
            snprintf(buf, sizeof(buf), "  %s (%d) is depended on by", Location::path(key).constData(), key);
            write(buf);
            const Set<uint32_t> deps = it->value<Set<uint32_t> >();
            for (Set<uint32_t>::const_iterator dit = deps.begin(); dit != deps.end(); ++dit) {
                snprintf(buf, sizeof(buf), "    %s (%d)", Location::path(*dit).constData(), *dit);
                write(buf);
                depsReversed[*dit].insert(key);
            }
            it->next();
        }
        for (Map<uint32_t, Set<uint32_t> >::const_iterator it = depsReversed.begin(); it != depsReversed.end(); ++it) {
            snprintf(buf, sizeof(buf), "  %s (%d) depends on", Location::path(it->first).constData(), it->first);
            write(buf);
            const Set<uint32_t> &deps = it->second;
            for (Set<uint32_t>::const_iterator dit = deps.begin(); dit != deps.end(); ++dit) {
                snprintf(buf, sizeof(buf), "    %s (%d)", Location::path(*dit).constData(), *dit);
                write(buf);
            }
        }
    }

    if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "symbols")) {
        matched = true;
        ScopedDB db = server->db(Server::Symbol, ReadWriteLock::Read);
        write(delimiter);
        write(server->databaseDir(Server::Symbol));
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        while (it->isValid()) {
            if (isAborted())
                return;
            const CursorInfo ci = it->value<CursorInfo>();
            const Location loc = Location::fromKey(it->key().data());
            write(loc, ci);
            it->next();
        }
    }

    if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "symbolnames")) {
        matched = true;
        ScopedDB db = server->db(Server::SymbolName, ReadWriteLock::Read, mIndexer->srcRoot());
        write(delimiter);
        write(server->databaseDir(Server::SymbolName));
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        char buf[1024];
        while (it->isValid()) {
            if (isAborted())
                return;
            snprintf(buf, sizeof(buf), "  %s:", it->key().byteArray().constData());
            write(buf);
            const Set<Location> locations = it->value<Set<Location> >();
            for (Set<Location>::const_iterator lit = locations.begin(); lit != locations.end(); ++lit) {
                const Location &loc = *lit;
                snprintf(buf, sizeof(buf), "    %s", loc.key().constData());
                write(buf);
            }
            it->next();
        }
    }

    if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "fileinfos")) {
        matched = true;
        ScopedDB db = server->db(Server::FileInformation, ReadWriteLock::Read, mIndexer->srcRoot());
        write(delimiter);
        write(server->databaseDir(Server::FileInformation));
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        char buf[1024];
        while (it->isValid()) {
            if (isAborted())
                return;

            const FileInformation fi = it->value<FileInformation>();
            const uint32_t fileId = *reinterpret_cast<const uint32_t*>(it->key().data());
            snprintf(buf, 1024, "  %s: last compiled: %s compile args: %s",
                     Location::path(fileId).constData(),
                     RTags::timeToString(fi.lastTouched).constData(),
                     ByteArray::join(fi.compileArgs, " ").constData());
            write(buf);
            it->next();
        }
    }

    if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "pch")) {
        matched = true;
        ScopedDB db = server->db(Server::PchUsrMaps, ReadWriteLock::Read, mIndexer->srcRoot());
        write(delimiter);
        write(server->databaseDir(Server::PchUsrMaps));
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        char buf[1024];
        while (it->isValid()) {
            if (isAborted())
                return;

            const PchUSRMap hash = it->value<PchUSRMap>();
            write(it->key().byteArray());
            snprintf(buf, 1024, "  %s", it->key().byteArray().constData());
            write(buf);
            for (PchUSRMap::const_iterator i = hash.begin(); i != hash.end(); ++i) {
                snprintf(buf, 1024, "    %s: %s", i->first.constData(), i->second.key().constData());
                write(buf);
            }

            it->next();
        }
    }

    if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "visitedfiles")) {
        matched = true;
        write(delimiter);
        write("visitedfiles");
        char buf[1024];
        const Set<uint32_t> visitedFiles = server->indexer()->visitedFiles();

        for (Set<uint32_t>::const_iterator it = visitedFiles.begin(); it != visitedFiles.end(); ++it) {
            const uint32_t id = *it;
            snprintf(buf, sizeof(buf), "  %s: %d", Location::path(id).constData(), id);
            write(buf);
        }
    }
    if (!matched) {
        write(alternatives);
    }

}
