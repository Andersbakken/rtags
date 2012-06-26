#include "StatusJob.h"
#include "Database.h"
#include "Server.h"
#include "RTags.h"
#include "Indexer.h"
#include <clang-c/Index.h>
#include <Rdm.h>
#include "CursorInfo.h"

const char *StatusJob::delimiter = "*********************************";
StatusJob::StatusJob(int i, const ByteArray &q)
    : Job(i, QueryJobPriority, WriteUnfiltered), query(q)
{
}

static inline ByteArray timeToString(time_t t)
{
#warning not done
    return ByteArray();
}

void StatusJob::execute()
{
    if (query.isEmpty() || query == "general") {
        ScopedDB db = Server::instance()->db(Server::General, ScopedDB::Read);
        write(delimiter);
        write(Server::databaseDir(Server::General));
        write("    version: " + ByteArray::number(db->value<int>("version")));

        const Map<Path, MakefileInformation> makefiles = db->value<Map<Path, MakefileInformation> >("makefiles");
        for (Map<Path, MakefileInformation>::const_iterator it = makefiles.begin(); it != makefiles.end(); ++it) {
            ByteArray out = "    " + it->first;
            out += " last touched: " + timeToString(it->second.lastTouched);
            if (!it->second.makefileArgs.isEmpty())
                out += " args: " + ByteArray::join(it->second.makefileArgs, " ");
            if (!it->second.extraFlags.isEmpty())
                out += " extra flags: " + ByteArray::join(it->second.extraFlags, " ");
            write(out);
        }
    }

    if (query.isEmpty() || query == "dependencies") {
        ScopedDB db = Server::instance()->db(Server::Dependency, ScopedDB::Read);
        write(delimiter);
        write(Server::databaseDir(Server::Dependency));
        RTags::Ptr<Iterator> it(db->createIterator());
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
            }
            it->next();
        }
    }

    if (query.isEmpty() || query == "symbols") {
        ScopedDB db = Server::instance()->db(Server::Symbol, ScopedDB::Read);
        write(delimiter);
        write(Server::databaseDir(Server::Symbol));
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        char buf[1024];
        while (it->isValid()) {
            if (isAborted())
                return;
            const CursorInfo ci = it->value<CursorInfo>();
            CXString kind = clang_getCursorKindSpelling(ci.kind);
            Location loc = Location::fromKey(it->key().data());
            snprintf(buf, sizeof(buf),
                     "  %s symbolName: %s kind: %s isDefinition: %s symbolLength: %d target: %s%s",
                     loc.key().constData(), ci.symbolName.constData(),
                     clang_getCString(kind), ci.isDefinition ? "true" : "false", ci.symbolLength,
                     ci.target.key().constData(),
                     ci.references.isEmpty() ? "" : " references:");
            clang_disposeString(kind);
            write(buf);
            for (Set<Location>::const_iterator rit = ci.references.begin(); rit != ci.references.end(); ++rit) {
                const Location &l = *rit;
                snprintf(buf, sizeof(buf), "    %s", l.key().constData());
                write(buf);
            }
            it->next();
        }
    }

    if (query.isEmpty() || query == "symbolnames") {
        ScopedDB db = Server::instance()->db(Server::SymbolName, ScopedDB::Read);
        write(delimiter);
        write(Server::databaseDir(Server::SymbolName));
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

    if (query.isEmpty() || query == "fileinfos") {
        ScopedDB db = Server::instance()->db(Server::FileInformation, ScopedDB::Read);
        write(delimiter);
        write(Server::databaseDir(Server::FileInformation));
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
                     timeToString(fi.lastTouched).constData(),
                     ByteArray::join(fi.compileArgs, " ").constData());
            write(buf);
            it->next();
        }
    }

    if (query.isEmpty() || query == "pch") {
        ScopedDB db = Server::instance()->db(Server::PCHUsrMaps, ScopedDB::Read);
        write(delimiter);
        write(Server::databaseDir(Server::PCHUsrMaps));
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

    if (query.isEmpty() || query == "fileids") {
        ScopedDB db = Server::instance()->db(Server::FileIds, ScopedDB::Read);
        write(delimiter);
        write(Server::databaseDir(Server::FileIds));
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        char buf[1024];
        while (it->isValid()) {
            snprintf(buf, 1024, "  %s: %d", it->key().byteArray().constData(), it->value<uint32_t>());
            write(buf);
            it->next();
        }
    }
    if (query.isEmpty() || query == "visitedFiles") {
        write(delimiter);
        write("visitedFiles");
        char buf[1024];
        const Set<uint32_t> visitedFiles = Server::instance()->indexer()->visitedFiles();

        for (Set<uint32_t>::const_iterator it = visitedFiles.begin(); it != visitedFiles.end(); ++it) {
            const uint32_t id = *it;
            snprintf(buf, sizeof(buf), "  %s: %d", Location::path(id).constData(), id);
            write(buf);
        }
    }
}
