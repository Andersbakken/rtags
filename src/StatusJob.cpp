#include "StatusJob.h"
#include "CursorInfo.h"
#include "Indexer.h"
#include "RTags.h"
#include "Server.h"
#include <clang-c/Index.h>

const char *StatusJob::delimiter = "*********************************";
StatusJob::StatusJob(const QueryMessage &q, const shared_ptr<Project> &project)
    : Job(q, WriteUnfiltered, project), query(q.query())
{
}

void StatusJob::execute()
{
//     bool matched = false;
//     Server *server = Server::instance();
//     if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "general")) {
//         matched = true;
//         ScopedDB database = server->db(Server::General, ReadWriteLock::Read);
//         write(delimiter);
//         write(server->databaseDir(Server::General));
//         write("    version: " + ByteArray::number(database->value<int>("version")));

//         const Map<Path, MakefileInformation> makefiles = database->value<Map<Path, MakefileInformation> >("makefiles");
//         for (Map<Path, MakefileInformation>::const_iterator it = makefiles.begin(); it != makefiles.end(); ++it) {
//             ByteArray out = "    " + it->first;
//             out += " last touched: " + RTags::timeToString(it->second.lastTouched, RTags::DateTime);
//             if (!it->second.makefileArgs.isEmpty())
//                 out += " args: " + ByteArray::join(it->second.makefileArgs, " ");
//             if (!it->second.extraFlags.isEmpty())
//                 out += " extra flags: " + ByteArray::join(it->second.extraFlags, " ");
//             write(out);
//         }
//     }

//     if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "fileids")) {
//         matched = true;
//         ScopedDB database = server->db(Server::FileIds, ReadWriteLock::Read);
//         write(delimiter);
//         write(server->databaseDir(Server::FileIds));
//         RTags::Ptr<Iterator> it(database->createIterator());
//         it->seekToFirst();
//         char buf[1024];
//         while (it->isValid()) {
//             snprintf(buf, 1024, "  %u: %s", it->value<uint32_t>(), it->key().byteArray().constData());
//             write(buf);
//             it->next();
//         }
//     }

//     const char *alternatives = "general|fileids|dependencies|fileinfos|symbols|symbolnames|visitedfiles|grfiles|gr";
//     shared_ptr<Project> proj = project();
//     if (!proj) {
//         if (!matched) {
//             write(alternatives);
//         }
//         return;
//     }

//     if (proj->indexer) {
//         if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "dependencies")) {
//             matched = true;
//             ScopedDB database = db(Project::Dependency, ReadWriteLock::Read);
//             write(delimiter);
//             write(proj->databaseDir(Project::Dependency));
//             RTags::Ptr<Iterator> it(database->createIterator());
//             Map<uint32_t, Set<uint32_t> > depsReversed;
//             it->seekToFirst();
//             char buf[1024];
//             while (it->isValid()) {
//                 if (isAborted())
//                     return;
//                 const uint32_t key = *reinterpret_cast<const uint32_t*>(it->key().data());
//                 snprintf(buf, sizeof(buf), "  %s (%d) is depended on by", Location::path(key).constData(), key);
//                 write(buf);
//                 const Set<uint32_t> deps = it->value<Set<uint32_t> >();
//                 for (Set<uint32_t>::const_iterator dit = deps.begin(); dit != deps.end(); ++dit) {
//                     snprintf(buf, sizeof(buf), "    %s (%d)", Location::path(*dit).constData(), *dit);
//                     write(buf);
//                     depsReversed[*dit].insert(key);
//                 }
//                 it->next();
//             }
//             for (Map<uint32_t, Set<uint32_t> >::const_iterator it = depsReversed.begin(); it != depsReversed.end(); ++it) {
//                 snprintf(buf, sizeof(buf), "  %s (%d) depends on", Location::path(it->first).constData(), it->first);
//                 write(buf);
//                 const Set<uint32_t> &deps = it->second;
//                 for (Set<uint32_t>::const_iterator dit = deps.begin(); dit != deps.end(); ++dit) {
//                     snprintf(buf, sizeof(buf), "    %s (%d)", Location::path(*dit).constData(), *dit);
//                     write(buf);
//                 }
//             }
//         }

//         if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "symbols")) {
//             matched = true;
//             ScopedDB database = db(Project::Symbol, ReadWriteLock::Read);
//             write(delimiter);
//             write(project()->databaseDir(Project::Symbol));
//             write(delimiter);
//             RTags::Ptr<Iterator> it(database->createIterator());
//             it->seekToFirst();
//             while (it->isValid()) {
//                 if (isAborted())
//                     return;
//                 const CursorInfo ci = it->value<CursorInfo>();
//                 const Location loc = Location::fromKey(it->key().data());
//                 write(loc, ci);
//                 it->next();
//             }
//         }

//         if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "symbolnames")) {
//             matched = true;
//             ScopedDB database = db(Project::SymbolName, ReadWriteLock::Read);
//             write(delimiter);
//             write(project()->databaseDir(Project::SymbolName));
//             write(delimiter);
//             RTags::Ptr<Iterator> it(database->createIterator());
//             it->seekToFirst();
//             char buf[1024];
//             while (it->isValid()) {
//                 if (isAborted())
//                     return;
//                 snprintf(buf, sizeof(buf), "  %s:", it->key().byteArray().constData());
//                 write(buf);
//                 const Set<Location> locations = it->value<Set<Location> >();
//                 for (Set<Location>::const_iterator lit = locations.begin(); lit != locations.end(); ++lit) {
//                     const Location &loc = *lit;
//                     snprintf(buf, sizeof(buf), "    %s", loc.key().constData());
//                     write(buf);
//                 }
//                 it->next();
//             }
//         }

//         if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "fileinfos")) {
//             matched = true;
//             ScopedDB database = db(Project::FileInformation, ReadWriteLock::Read);
//             write(delimiter);
//             write(project()->databaseDir(Project::FileInformation));
//             write(delimiter);
//             RTags::Ptr<Iterator> it(database->createIterator());
//             it->seekToFirst();
//             char buf[1024];
//             while (it->isValid()) {
//                 if (isAborted())
//                     return;

//                 const FileInformation fi = it->value<FileInformation>();
//                 const uint32_t fileId = *reinterpret_cast<const uint32_t*>(it->key().data());
//                 snprintf(buf, 1024, "  %s: last compiled: %s compile args: %s",
//                          Location::path(fileId).constData(),
//                          RTags::timeToString(fi.lastTouched, RTags::DateTime).constData(),
//                          ByteArray::join(fi.compileArgs, " ").constData());
//                 write(buf);
//                 it->next();
//             }
//         }

//         if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "visitedfiles")) {
//             matched = true;
//             write(delimiter);
//             write("visitedfiles");
//             char buf[1024];
//             const Set<uint32_t> visitedFiles = proj->indexer->visitedFiles();

//             for (Set<uint32_t>::const_iterator it = visitedFiles.begin(); it != visitedFiles.end(); ++it) {
//                 const uint32_t id = *it;
//                 snprintf(buf, sizeof(buf), "  %s: %d", Location::path(id).constData(), id);
//                 write(buf);
//             }
//         }
//     }
//     if (query.isEmpty() || !strcasecmp(query.nullTerminated(), "grfiles")) {
//         matched = true;
    //     ScopedDB database = db(Project::GRFiles, ReadWriteLock::Read);
    //     write(delimiter);
    //     write(project()->databaseDir(Project::GRFiles));
    //     write(delimiter);

    //     RTags::Ptr<Iterator> it(database->createIterator());
    //     it->seekToFirst();
    //     char buf[1024];
    //     while (it->isValid()) {
    //         if (isAborted())
    //             return;
    //         time_t time = it->value<time_t>();
    //         snprintf(buf, sizeof(buf), "    %s %s", it->key().byteArray().nullTerminated(),
    //                  time ? RTags::timeToString(time, RTags::DateTime).constData() : "");
    //         write(buf);
    //         it->next();
    //     }
    // }
    // if ((query.isEmpty() || !strcasecmp(query.nullTerminated(), "gr")) && project()->grtags->flags() & GRTags::Parse)  {
    //     matched = true;
    //     ScopedDB database = db(Project::GR, ReadWriteLock::Read);
    //     write(delimiter);
    //     write(project()->databaseDir(Project::GR));
    //     write(delimiter);
    //     RTags::Ptr<Iterator> it(database->createIterator());
    //     it->seekToFirst();
    //     char buf[1024];
    //     while (it->isValid()) {
    //         if (isAborted())
    //             return;
    //         snprintf(buf, sizeof(buf), "  %s:", it->key().byteArray().constData());
    //         write(buf);
    //         const Map<Location, bool> locations = it->value<Map<Location, bool> >();
    //         for (Map<Location, bool>::const_iterator lit = locations.begin(); lit != locations.end(); ++lit) {
    //             snprintf(buf, sizeof(buf), "    %s%s", lit->first.key().constData(), lit->second ? "ref" : "");
    //             write(buf);
    //         }
    //         it->next();
    //     }
    // }

    // if (!matched) {
    //     write(alternatives);
    // }

}
