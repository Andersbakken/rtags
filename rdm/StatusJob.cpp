#include "StatusJob.h"
#include "Server.h"
#include "RTags.h"
#include "Indexer.h"
#include <clang-c/Index.h>
#include <Rdm.h>

StatusJob::StatusJob(int i, const QByteArray &q)
    : Job(i, WriteUnfiltered), query(q)
{
}

void StatusJob::run()
{
    if (query.isEmpty() || query == "general") {
        leveldb::DB *db = Server::instance()->db(Server::General);
        write(Server::databaseDir(Server::General));
        write("    version: " + QByteArray::number(Rdm::readValue<int>(db, "version")));
    }

    if (query.isEmpty() || query == "dependencies") {
        leveldb::DB *db = Server::instance()->db(Server::Dependency);
        write(Server::databaseDir(Server::Dependency));
        leveldb::Iterator* it = db->NewIterator(leveldb::ReadOptions());
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

    if (query.isEmpty() || query == "symbols") {
        leveldb::DB *db = Server::instance()->db(Server::Symbol);
        write(Server::databaseDir(Server::Symbol));
        leveldb::Iterator* it = db->NewIterator(leveldb::ReadOptions());
        it->SeekToFirst();
        char buf[1024];
        memcpy(buf, "  ", 2);
        while (it->Valid()) {
            memcpy(buf + 2, it->key().data(), it->key().size());
            const CursorInfo ci = Rdm::readValue<CursorInfo>(it);
            CXString kind = clang_getCursorKindSpelling(ci.kind);
            snprintf(buf + 2 + it->key().size(), sizeof(buf) - it->key().size() - 3,
                     " kind: %s symbolLength: %d symbolName: %s target: %s%s",
                     clang_getCString(kind), ci.symbolLength, ci.symbolName.constData(),
                     ci.target.key(Location::Padded).constData(),
                     ci.references.isEmpty() ? "" : " references:");
            clang_disposeString(kind);
            write(buf);
            foreach(const Location &loc, ci.references) {
                const int w = snprintf(buf + 2, sizeof(buf) - 4, "  %s",
                                       loc.key(Location::Padded).constData());
                write(QByteArray(buf, w + 2));
            }
            it->Next();
        }
        delete it;
    }

    if (query.isEmpty() || query == "symbolnames") {
        leveldb::DB *db = Server::instance()->db(Server::SymbolName);
        write(Server::databaseDir(Server::SymbolName));
        leveldb::Iterator* it = db->NewIterator(leveldb::ReadOptions());
        it->SeekToFirst();
        char buf[1024];
        memcpy(buf, "  ", 2);
        while (it->Valid()) {
            memcpy(buf + 2, it->key().data(), it->key().size());
            memcpy(buf + 2 + it->key().size(), ":", 2);
            write(buf);
            const QSet<Location> locations = Rdm::readValue<QSet<Location> >(it);
            memcpy(buf + 2, "  ", 2);
            foreach (const Location &loc, locations) {
                QByteArray key = loc.key(Location::Padded);
                memcpy(buf + 4, key.constData(), key.size() + 1);
                write(buf);
            }
            it->Next();
        }
        delete it;
    }

    if (query.isEmpty() || query == "fileinfos") {
        leveldb::DB *db = Server::instance()->db(Server::FileInformation);
        write(Server::databaseDir(Server::FileInformation));
        leveldb::Iterator* it = db->NewIterator(leveldb::ReadOptions());
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

    if (query.isEmpty() || query == "pch") {
        // ### needs to be done
    }
    finish();
}
