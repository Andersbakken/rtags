#include "StatusJob.h"
#include "Server.h"
#include "RTags.h"
#include "Indexer.h"
#include <clang-c/Index.h>
#include <Rdm.h>
#include "CursorInfo.h"

StatusJob::StatusJob(int i, const QByteArray &q)
    : Job(i, WriteUnfiltered), query(q)
{
}

void StatusJob::execute()
{
    const char *delimiter = "*********************************";
    if (query.isEmpty() || query == "general") {
        Database *db = Server::instance()->db(Server::General);
        write(delimiter);
        write(Server::databaseDir(Server::General));
        write("    version: " + QByteArray::number(db->value<int>("version")));
    }

    if (query.isEmpty() || query == "dependencies") {
        Database *db = Server::instance()->db(Server::Dependency);
        write(delimiter);
        write(Server::databaseDir(Server::Dependency));
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        char buf[1024];
        while (it->isValid()) {
            if (isAborted())
                return;
            const quint32 key = *reinterpret_cast<const quint32*>(it->key().data());
            snprintf(buf, sizeof(buf), "  %s (%d) is depended on by", Location::path(key).constData(), key);
            write(buf);
            const QSet<quint32> deps = it->value<QSet<quint32> >();
            foreach (quint32 p, deps) {
                snprintf(buf, sizeof(buf), "    %s (%d)", Location::path(p).constData(), p);
                write(buf);
            }
            it->next();
        }
    }

    if (query.isEmpty() || query == "symbols") {
        Database *db = Server::instance()->db(Server::Symbol);
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
                     "  %s symbolName: %s kind: %s symbolLength: %d target: %s%s",
                     loc.key().constData(), ci.symbolName.constData(),
                     clang_getCString(kind), ci.symbolLength,
                     ci.target.key().constData(),
                     ci.references.isEmpty() ? "" : " references:");
            clang_disposeString(kind);
            write(buf);
            foreach(const Location &loc, ci.references) {
                snprintf(buf, sizeof(buf), "    %s", loc.key().constData());
                write(buf);
            }
            it->next();
        }
    }

    if (query.isEmpty() || query == "symbolnames") {
        Database *db = Server::instance()->db(Server::SymbolName);
        write(delimiter);
        write(Server::databaseDir(Server::SymbolName));
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        char buf[1024];
        while (it->isValid()) {
            if (isAborted())
                return;
            snprintf(buf, sizeof(buf), "  %s:", std::string(it->key().data(), it->key().size()).c_str());
            write(buf);
            const QSet<Location> locations = it->value<QSet<Location> >();
            foreach (const Location &loc, locations) {
                snprintf(buf, sizeof(buf), "    %s", loc.key().constData());
                write(buf);
            }
            it->next();
        }
    }

    if (query.isEmpty() || query == "fileinfos") {
        Database *db = Server::instance()->db(Server::FileInformation);
        write(delimiter);
        write(Server::databaseDir(Server::FileInformation));
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        char buf[1024];
        while (it->isValid()) {
            if (isAborted())
                return;

            const FileInformation fi = it->value<FileInformation>();
            snprintf(buf, 1024, "  %s: last compiled: %s compile args: %s",
                     Location::fromKey(it->key().data()).key().constData(),
                     QDateTime::fromTime_t(fi.lastTouched).toString().toLocal8Bit().constData(),
                     RTags::join(fi.compileArgs).constData());
            write(buf);
            it->next();
        }
    }

    if (query.isEmpty() || query == "pch") {
        // ### needs to be done
    }

    if (query.isEmpty() || query == "fileids") {
        Database *db = Server::instance()->db(Server::FileIds);
        write(delimiter);
        write(Server::databaseDir(Server::FileIds));
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        char buf[1024];
        while (it->isValid()) {
            snprintf(buf, 1024, "  %s: %d", std::string(it->key().data(), it->key().size()).c_str(), it->value<quint32>());
            write(buf);
            it->next();
        }
    }
}
