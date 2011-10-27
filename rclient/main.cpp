#include <sstream>
#include <leveldb/db.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <QtCore>
#include <getopt.h>
#include <RTags.h>
#include <CursorKey.h>

using namespace RTags;
QSet<QByteArray> printed;
static inline void print(const QByteArray &out)
{
    if (!printed.contains(out)) {
        printed.insert(out);
        printf("%s\n", out.constData());
    }
}
static inline bool maybeDict(leveldb::DB *db, const std::string &key,
                             bool (*func)(leveldb::DB *db, const std::string &key))
{
    bool ret = false;
    std::string val;
    db->Get(leveldb::ReadOptions(), "d:" + key, &val);
    if (!val.empty()) {
        foreach(const QByteArray &k, QByteArray::fromRawData(val.c_str(), val.size()).split('\0')) {
            if (!k.isEmpty()) {
                ret = func(db, std::string(k.constData(), k.size())) || ret;
            }
        }
    }
    return ret;
}

static inline bool followSymbol(leveldb::DB *db, const std::string &key)
{
    std::string val;
    db->Get(leveldb::ReadOptions(), key, &val);
    if (!val.empty()) {
        QByteArray referredTo;
        const QByteArray v = QByteArray::fromRawData(val.c_str(), val.size());
        QDataStream ds(v);
        ds >> referredTo;
        if (!referredTo.isEmpty()) {
            print(referredTo);
        }
        return true;
    }
    return false;
}

static inline bool findReferences(leveldb::DB *db, const std::string &key)
{
    std::string val;
    db->Get(leveldb::ReadOptions(), key, &val);
    if (!val.empty()) {
        QByteArray referredTo;
        const QByteArray v = QByteArray::fromRawData(val.c_str(), val.size());
        QDataStream ds(v);
        QSet<Cursor> references;
        ds >> referredTo >> references;
        if (referredTo != key.c_str() && references.isEmpty()) {
            return findReferences(db, std::string(referredTo.constData(), referredTo.size()));
        }
        foreach(const Cursor &r, references) {
            print(r.key.toString());
        }
        return true;
    }
    return false;
}

static inline void usage(const char* argv0, FILE *f)
{
    fprintf(f,
            "%s [options]...\n"
            "  --help|-h                  Display this help\n"
            "  --follow-symbol|-f [arg]   Follow this symbol (e.g. /tmp/main.cpp:32:1)\n"
            "  --references|-r [arg]      Print references of symbol at arg\n"
            "  --list-symbols|-l [arg]    Print out symbols matching arg\n"
            "  --db-file|-d [arg]         Use this database file\n"
            "  --detect-db|-D             Find .rtags.db based on path\n"
            "                             (default when no -d options are specified)\n",
            argv0);
}

int main(int argc, char** argv)
{
    struct option longOptions[] = {
        { "help", 0, 0, 'h' },
        { "follow-symbol", 1, 0, 'f' },
        { "db", 1, 0, 'd' },
        { "detect-db", 0, 0, 'D' },
        { "find-references", 1, 0, 'r' },
        // { "recursive-references", 1, 0, 'R' },
        // { "max-recursion-reference-depth", 1, 0, 'x' },
        { "list-symbols", 1, 0, 'l' },
        { 0, 0, 0, 0 },
    };
    const char *shortOptions = "hf:d:r:l:D";

    QList<QByteArray> dbPaths;

    enum Mode {
        None,
        FollowSymbol,
        References,
        // RecursiveReferences,
        ListSymbols
    } mode = None;
    int idx, longIndex;
    std::string arg;
    while ((idx = getopt_long(argc, argv, shortOptions, longOptions, &longIndex)) != -1) {
        switch (idx) {
        case '?':
            usage(argv[0], stderr);
            return 1;
        case 'h':
            usage(argv[0], stdout);
            return 0;
        case 'f':
            if (mode != None) {
                fprintf(stderr, "Mode is already set\n");
                return 1;
            }
            arg = optarg;
            mode = FollowSymbol;
            break;
        case 'r':
            arg = optarg;
            if (mode != None) {
                fprintf(stderr, "Mode is already set\n");
                return 1;
            }
            mode = References;
            break;
        case 'D': {
            const QByteArray db = findRtagsDb();
            if (!db.isEmpty()) {
                dbPaths.append(db);
            }
            break; }
        case 'd':
            dbPaths.append(optarg);
            break;
        case 'l':
            if (mode != None) {
                fprintf(stderr, "Mode is already set\n");
                return 1;
            }
            mode = ListSymbols;
            arg = optarg;
            break;
        }
    }
    if (dbPaths.isEmpty())
        dbPaths.append(findRtagsDb());

    if (dbPaths.isEmpty()) {
        fprintf(stderr, "No databases specified\n");
        return 1;
    }
    foreach(const QByteArray &dbPath, dbPaths) {
        leveldb::DB* db;
        if (!leveldb::DB::Open(leveldb::Options(), dbPath.constData(), &db).ok()) {
            fprintf(stderr, "Unable to open db %s\n", dbPath.constData());
            continue;
        }
        LevelDBScope scope(db);

        switch (mode) {
        case None:
            return 1;
        case FollowSymbol:
            if (followSymbol(db, arg) || maybeDict(db, arg, followSymbol))
                return 0;
            break;
        case References:
            if (!findReferences(db, arg))
                maybeDict(db, arg, findReferences);
            break;
        case ListSymbols: {
            std::string val;
            leveldb::Iterator *it = db->NewIterator(leveldb::ReadOptions());
            it->Seek("d:");
            while (it->Valid()) {
                std::string k = it->key().ToString();
                // leveldb::Slice k = it->key();
                // leveldb::Slice v = it->value();
                // for (int i=0; i<k.size(); ++i) {
                //     printf("'%c'(%d)", k.data()[i], i);
                // }
                // printf("\n");

                // printf("%s:%s\n", k.data(), v.data());
                if (k.empty() || strncmp(k.c_str(), "d:", 2))
                    break;
                if (arg.empty() || strstr(k.c_str(), arg.c_str())) {
                    printf("%s\n", k.c_str() + 2);
                }
                it->Next();
            }
            delete it;
            break; }
        }
    }

    return 0;
}
