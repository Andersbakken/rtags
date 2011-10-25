#include <sstream>
#include <leveldb/db.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <QtCore>
#include <getopt.h>

static inline std::string findRtagsDb(const std::string& filename)
{
    char buffer[500];
    if (getcwd(buffer, 500)) {
        char *slash;
        while ((slash = strrchr(buffer, '/'))) {
            // ### this is awful
            struct ::stat s;
            std::string path(buffer);
            path += filename;
            //printf("Testing [%s]\n", path.c_str());
            if (stat(path.c_str(), &s) >= 0)
                return path;
            *slash = '\0';
        }
    }
    return std::string();
}

class Scope
{
public:
    Scope(leveldb::DB *d)
        : db(d)
    {}
    ~Scope()
    {
        delete db;
    }
private:
    leveldb::DB *db;
};

static inline void maybeDict(leveldb::DB *db, const std::string &key,
                             bool (*func)(leveldb::DB *db, const std::string &key))
{
    std::string val;
    db->Get(leveldb::ReadOptions(), "d:" + key, &val);
    if (!val.empty()) {
        foreach(const QByteArray &k, QByteArray::fromRawData(val.c_str(), val.size()).split('\0')) {
            if (!k.isEmpty()) {
                func(db, std::string(k.constData(), k.size()));
            }
        }
    }
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
            printf("%s\n", referredTo.constData());
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
        QSet<QByteArray> references;
        ds >> referredTo >> references;
        if (referredTo != key.c_str() && references.isEmpty()) {
            return findReferences(db, std::string(referredTo.constData(), referredTo.size()));
        }
        foreach(const QByteArray &r, references) {
            printf("%s\n", r.constData());
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
            "  --db-file|-d [arg]         Use this database file\n",
            argv0);
}

int main(int argc, char** argv)
{
    if (argc < 3)
        return 1;

    struct option longOptions[] = {
        { "help", 0, 0, 'h' },
        { "follow-symbol", 1, 0, 'f' },
        { "db-file", 1, 0, 'f' },
        { "find-references", 1, 0, 'r' },
        // { "recursive-references", 1, 0, 'R' },
        // { "max-recursion-reference-depth", 1, 0, 'x' },
        { "list-symbols", 1, 0, 'l' },
        { 0, 0, 0, 0 },
    };
    const char *shortOptions = "hf:d:r:l:";

    std::string dbFile = findRtagsDb("/.rtags.db");


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
        case 'd':
            dbFile = optarg;
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

    if (dbFile.empty())
        return 1;
    leveldb::DB* db;
    if (!leveldb::DB::Open(leveldb::Options(), dbFile, &db).ok()) {
        fprintf(stderr, "Unable to open db %s\n", dbFile.c_str());
        return 1;
    }
    Scope scope(db);

    switch (mode) {
    case None:
        return 1;
    case FollowSymbol:
        if (!followSymbol(db, arg))
            maybeDict(db, arg, followSymbol);
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

    return 0;
}
