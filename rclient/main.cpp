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
typedef bool (*Handler)(leveldb::DB *db, const std::string &key);
static inline bool maybeDict(leveldb::DB *db, const std::string &key, Handler handler)
{
    bool ret = false;
    std::string val;
    db->Get(leveldb::ReadOptions(), "d:" + key, &val);
    if (!val.empty()) {
        foreach(const QByteArray &k, QByteArray::fromRawData(val.c_str(), val.size()).split('\0')) {
            if (!k.isEmpty()) {
                ret = handler(db, std::string(k.constData(), k.size())) || ret;
            }
        }
    }
    return ret;
}

static bool maybeResolveAndMaybeDict(leveldb::DB *db, const std::string &key, Handler handler)
{
    if (handler(db, key))
        return true;
    if (!key.empty() && key.at(0) != '/') {
        std::string file;
        unsigned l, c;
        if (parseLocation(key, file, l, c)) {
            const Path resolved = Path::resolved(file.c_str());
            if (resolved.exists()) {
                QVarLengthArray<char, 256> buf(resolved.size() + 32);
                snprintf(buf.data(), buf.size(), "%s:%d:%d:", resolved.constData(), l, c);
                if (handler(db, buf.constData())) {
                    return true;
                }
            }
        }
    }
    return maybeDict(db, key, handler);
}

static inline void print(const QByteArray &out)
{
    if (!printed.contains(out)) {
        printed.insert(out);
        printf("%s\n", out.constData());
    }
}

static inline bool printSymbol(leveldb::DB *, const std::string &loc)
{
    if (!loc.empty()) {
        print(QByteArray(loc.c_str()));
        return true;
    }
    return false;
}

static inline bool followSymbol(leveldb::DB *db, const std::string &key)
{
    std::string val;
    db->Get(leveldb::ReadOptions(), key, &val);
    // qDebug() << key.c_str() << val.size();
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
            QByteArray out = r.key.toString();
            if (!r.containingFunction.isEmpty()) {
                out += ' ';
                out += r.containingFunction.toByteArray();
            }

            print(out);
        }
        return true;
    }
    return false;
}

static inline void usage(const char* argv0, FILE *f)
{
    fprintf(f,
            "%s [options]...\n"
            "  --help|-h                   Display this help\n"
            "  --db-file|-d [arg]          Use this database file\n"
            "  --print-detected-db-path|-p Print out the detected database path\n"
            "  --detect-db|-D              Find .rtags.db based on path\n"
            "                              (default when no -d options are specified)\n"
            "  Modes\n"
            "  --follow-symbol|-f [arg]    Follow this symbol (e.g. /tmp/main.cpp:32:1)\n"
            "  --references|-r [arg]       Print references of symbol at arg\n"
            "  --list-symbols|-l [arg]     Print out symbols matching arg\n"
            "  --find-symbols|-s [arg]    Print out symbols matching arg\n",
            argv0);
}

int main(int argc, char** argv)
{
    struct option longOptions[] = {
        { "help", 0, 0, 'h' },
        { "follow-symbol", 1, 0, 'f' },
        { "db", 1, 0, 'd' },
        { "print-detected-db-path", 0, 0, 'p' },
        { "find-references", 1, 0, 'r' },
        { "find-symbols", 1, 0, 's' },
        // { "recursive-references", 1, 0, 'R' },
        // { "max-recursion-reference-depth", 1, 0, 'x' },
        { "find-db", 0, 0, 'F' },
        { "list-symbols", 1, 0, 'l' },
        { 0, 0, 0, 0 },
    };
    const char *shortOptions = "hf:d:r:l:Dps:";

    QList<QByteArray> dbPaths;

    enum Mode {
        None,
        FollowSymbol,
        References,
        FindSymbols,
        ListSymbols
        // RecursiveReferences,
    } mode = None;
    int idx, longIndex;
    std::string arg;
    while ((idx = getopt_long(argc, argv, shortOptions, longOptions, &longIndex)) != -1) {
        switch (idx) {
        case '?':
            usage(argv[0], stderr);
            return 1;
        case 'p': {
            const QByteArray db = findRtagsDb();
            if (!db.isEmpty()) {
                printf("%s\n", db.constData());
            } else {
                char buffer[500];
                getcwd(buffer, 500);
                fprintf(stderr, "No db found for %s\n", buffer);
            }
            return 0; }
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
            if (optarg && strlen(optarg))
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
        case 's':
            if (mode != None) {
                fprintf(stderr, "Mode is already set\n");
                return 1;
            }
            mode = FindSymbols;
            arg = optarg;
            break;
        }
    }
    if (dbPaths.isEmpty()) {
        QByteArray db = findRtagsDb();
        if (db.isEmpty() && !arg.empty())
            db = findRtagsDb(arg.c_str());
        if (!db.isEmpty())
            dbPaths.append(db);
    }

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
            usage(argv[0], stderr);
            fprintf(stderr, "No mode selected\n");
            return 1;
        case FollowSymbol:
            if (maybeResolveAndMaybeDict(db, arg, followSymbol))
                return 0; // we only want the first one here
            break;
        case References:
            maybeResolveAndMaybeDict(db, arg, findReferences);
            break;
        case FindSymbols:
            maybeDict(db, arg, printSymbol);
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
