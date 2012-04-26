#include "StatusJob.h"
#include "Server.h"
#include "RTags.h"
#include "Indexer.h"
#include <clang-c/Index.h>
#include <Rdm.h>
#include "LevelDB.h"

StatusJob::StatusJob(int i, const QByteArray &q)
    : Job(i, WriteUnfiltered), query(q)
{
}

void StatusJob::run()
{
    if (query.isEmpty() || query == "general") {
        LevelDB db;
        if (db.open(Server::General, LevelDB::ReadOnly)) {
            write(Server::databaseDir(Server::General));
            write("    version: " + QByteArray::number(Rdm::readValue<int>(db.db(), "version")));
        }
    }

    if (query.isEmpty() || query == "dependencies") {
        LevelDB db;
        if (db.open(Server::Dependency, LevelDB::ReadOnly)) {
            write(Server::databaseDir(Server::Dependency));
            leveldb::Iterator* it = db.db()->NewIterator(leveldb::ReadOptions());
            it->SeekToFirst();
            char buf[1024];
            memcpy(buf, "  ", 2);
            while (it->Valid()) {
                memcpy(buf + 2, it->key().data(), it->key().size());
                memcpy(buf + 2 + it->key().size(), " is depended on by:", 20);
                write(buf);
                const QSet<Path> deps = Rdm::readValue<QSet<Path> >(it);
                memcpy(buf + 2, "  ", 2);
                foreach (const Path &p, deps) {
                    memcpy(buf + 4, p.constData(), p.size() + 1);
                    write(buf);
                }
                it->Next();
            }
            delete it;
        }
    }

    if (query.isEmpty() || query == "symbols") {
        LevelDB db;
        if (db.open(Server::Symbol, LevelDB::ReadOnly)) {
            write(Server::databaseDir(Server::Symbol));
            leveldb::Iterator* it = db.db()->NewIterator(leveldb::ReadOptions());
            it->SeekToFirst();
            char buf[1024];
            memcpy(buf, "  ", 2);
            while (it->Valid()) {
                memcpy(buf + 2, it->key().data(), it->key().size());
                const Rdm::CursorInfo ci = Rdm::readValue<Rdm::CursorInfo>(it);
                CXString kind = clang_getCursorKindSpelling(ci.kind);
                snprintf(buf + 2 + it->key().size(), sizeof(buf) - it->key().size() - 3,
                         " kind: %s symbolLength: %d target: %s%s",
                         clang_getCString(kind), ci.symbolLength, ci.target.key(RTags::Location::Padded).constData(),
                         ci.references.isEmpty() ? "" : " references:");
                clang_disposeString(kind);
                write(buf);
                foreach(const RTags::Location &loc, ci.references) {
                    const int w = snprintf(buf + 2, sizeof(buf) - 4, "  %s",
                                           loc.key(RTags::Location::Padded).constData());
                    write(QByteArray(buf, w + 2));
                }
                it->Next();
            }
            delete it;
        }
    }

    if (query.isEmpty() || query == "symbolnames") {
        LevelDB db;
        if (db.open(Server::SymbolName, LevelDB::ReadOnly)) {
            write(Server::databaseDir(Server::SymbolName));
            leveldb::Iterator* it = db.db()->NewIterator(leveldb::ReadOptions());
            it->SeekToFirst();
            char buf[1024];
            memcpy(buf, "  ", 2);
            while (it->Valid()) {
                memcpy(buf + 2, it->key().data(), it->key().size());
                memcpy(buf + 2 + it->key().size(), ":", 2);
                write(buf);
                const QSet<RTags::Location> locations = Rdm::readValue<QSet<RTags::Location> >(it);
                memcpy(buf + 2, "  ", 2);
                foreach (const RTags::Location &loc, locations) {
                    QByteArray key = loc.key(RTags::Location::Padded);
                    memcpy(buf + 4, key.constData(), key.size() + 1);
                    write(buf);
                }
                it->Next();
            }
            delete it;
        }
    }

    if (query.isEmpty() || query == "fileinfos") {
        LevelDB db;
        if (db.open(Server::FileInformation, LevelDB::ReadOnly)) {
            write(Server::databaseDir(Server::FileInformation));
            leveldb::Iterator* it = db.db()->NewIterator(leveldb::ReadOptions());
            it->SeekToFirst();
            char buf[1024];
            memcpy(buf, "  ", 2);
            while (it->Valid()) {
                memcpy(buf + 2, it->key().data(), it->key().size());
                const FileInformation fi = Rdm::readValue<FileInformation>(it);
                snprintf(buf + 2 + it->key().size(), sizeof(buf) - 3 - it->key().size(),
                         ": %s [%s]", QDateTime::fromTime_t(fi.lastTouched).toString().toLocal8Bit().constData(),
                         RTags::join(fi.compileArgs).constData());
                write(buf);
                it->Next();
            }
            delete it;
        }
    }

    if (query.isEmpty() || query == "pch") {
        // ### needs to be done
    }
    finish();
}
