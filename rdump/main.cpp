#include <sstream>
#include <leveldb/db.h>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <RTags.h>
#include <QtCore>
#include <GccArguments.h>
#include <FileDB.h>

using namespace RTags;

enum Type {
    Symbol = 0x01,
    Reference = 0x02,
    Dependency = 0x04,
    Dict = 0x08,
    Files = 0x10,
    Raw = 0x20,
    All = 0xff
};

static inline void dumpDatabase(const std::string& filename, int type)
{
//     leveldb::DB* db;
//     const leveldb::Status status = leveldb::DB::Open(leveldb::Options(), filename, &db);
//     if (!status.ok()) {
//         fprintf(stderr, "Unable to open db [%s]: %s\n", filename.c_str(), status.ToString().c_str());
//         return;
//     }
//     leveldb::Iterator* it = db->NewIterator(leveldb::ReadOptions());
//     for (it->SeekToFirst(); it->Valid(); it->Next()) {
//         // unsigned line = 0, col = 0;;
//         // std::string mod = it->key().ToString();
//         // printf("%d\n", parseLocation(it->key().ToString(), mod, line, col));
//         // printf("%s %d %d\n", mod.c_str(), line, col);
//         const std::string key = it->key().ToString();
//         std::string fileName;
//         unsigned line, col;
//         // qDebug() << key.c_str();
//         if (type & Raw) {
//             printf("[%s]\n", key.c_str());
//         }
//         if (parseLocation(key, fileName, line, col)) {
//             leveldb::Slice val = it->value();
//             const QByteArray v = QByteArray::fromRawData(val.data(), val.size());
//             QByteArray mapsTo;
//             QSet<Cursor> references;
//             QDataStream ds(v);
//             ds >> mapsTo >> references;
//             // printf("%s (%s) maps to %s (%s)\n",
//             //        key.c_str(),
//             //        symbolNameAt(key).c_str(),
//             //        it->value().ToString().c_str(),
//             //        symbolNameAt(it->value().ToString()).c_str());
//             if (type & Symbol && !mapsTo.isEmpty()) {
//                 printf("%s maps to %s\n",
//                        key.c_str(),
//                        mapsTo.constData());
//             }
//             if (type & Reference) {
//                 // printf("Balle %d %d %d %d\n", references.size(), v.size(), val.size(),
//                 //        it->value().ToString().size());
//                 foreach(const Cursor &r, references) {
//                     printf("%s refers to %s\n", r.key.toString().constData(), key.c_str());
//                 }
//             }
//         } else if (key.substr(0, 2) == "d:") { // dict
//             if (type & Dict) {
//                 leveldb::Slice val = it->value();
//                 const QByteArray v = QByteArray::fromRawData(val.data(), val.size());
//                 QSet<QByteArray> strings; // these are streamed out as
//                                           // AtomicString but they're streamed
//                                           // as a single QByteArray so it should
//                                           // be fine
//                 QDataStream ds(v);
//                 ds >> strings;
//                 printf("dict entry %s:", key.substr(2).c_str());
//                 bool first = true;
//                 foreach(const QByteArray &loc, strings) {
//                     if (!first) {
//                         printf(" ::");
//                     } else {
//                         first = false;
//                     }
//                     printf(" %s", loc.constData());
//                 }
//                 printf("\n");
//             }
//         } else if (key.substr(0, 2) == "f:") { // dependency
//             if (type & Dependency) {
//                 const QByteArray ba = QByteArray::fromRawData(it->value().data(),
//                                                               it->value().size());
//                 QDataStream ds(ba);
//                 GccArguments args;
//                 ds >> args;
//                 quint64 lastModified;
//                 ds >> lastModified;
//                 time_t tt = static_cast<time_t>(lastModified);
//                 printf("%s %s %s", key.c_str() + 2, args.raw().constData(), ctime(&tt));
//                 QHash<QByteArray, quint64> dependencies;
//                 ds >> dependencies;
//                 for (QHash<QByteArray, quint64>::const_iterator it = dependencies.begin();
//                      it != dependencies.end(); ++it) {
//                     tt = static_cast<time_t>(it.value());
//                     printf("  %s %s", it.key().constData(), ctime(&tt));
//                 }
//             }
//         } else if (key == "files") {
//             if (type & Files) {
//                 QSet<Path> files;
//                 const QByteArray ba = QByteArray::fromRawData(it->value().data(),
//                                                               it->value().size());
//                 QDataStream ds(ba);
//                 ds >> files;
//                 foreach(const Path &path, files) {
//                     printf("%s\n", path.constData());
//                 }
//             }
//         }
//     }
//     delete it;
//     delete db;
}

/*
static inline bool writeExpect(const std::string& filename)
{
    leveldb::DB* db;
    if (!leveldb::DB::Open(leveldb::Options(), filename, &db).ok()) {
        fprintf(stderr, "Unable to open db %s\n", filename.c_str());
        return false;
    }

    FILE* f = fopen("expect.txt", "w");
    if (!f) {
        fprintf(stderr, "Unable to open expect.txt for writing\n");
        delete db;
        return false;
    }

    leveldb::Iterator* it = db->NewIterator(leveldb::ReadOptions());
    for (it->SeekToFirst(); it->Valid(); it->Next()) {
        const std::string key = it->key().ToString();
        std::string fileName;
        unsigned line, col;
        if (parseLocation(key, fileName, line, col)) {
            const leveldb::Slice value = it->value();
            const QByteArray data = QByteArray::fromRawData(value.data(), value.size());
            QDataStream ds(data);
            QByteArray v;
            ds >> v;
            if (!v.isEmpty()) {
                fprintf(f, "--follow-symbol\n%s\n%s\n\n",
                        removePath(key).c_str(),
                        removePath(v).constData());
            }
        }
    }

    fclose(f);

    delete it;
    delete db;

    printf("expect.txt written\n");

    return true;
}
*/

static inline int syntax(int opt, const char* app)
{
    fprintf(stderr, "Syntax: %s [-e] [-t type] <database>\n", app);
    return opt == 'h' ? 0 : 1;
}

static inline bool parseType(const char* a, int* type)
{
    *type = 0;
    const char* t = a;
    const char* end = t + strlen(t);
    for (; t != end; ++t) {
        switch (*t) {
        case 's':
            *type |= Symbol;
            break;
        case 'r':
            *type |= Reference;
            break;
        case 'd':
            *type |= Dependency;
            break;
        case 'f':
            *type |= Files;
            break;
        case 'i':
            *type |= Dict;
            break;
        case 'a':
            *type |= All;
            break;
        case 'R':
            *type |= Raw;
            break;
        default:
            return false;
        }
    }
    return true;
}

int main(int argc, char** argv)
{
    bool createExpect = false;
    int opt, type = All;
    while ((opt = getopt(argc, argv, "eht:")) != -1) {
        switch (opt) {
        case 'e':
            createExpect = true;
            break;
        case 't': {
            if (parseType(optarg, &type))
                break;
            fprintf(stderr, "Unable to parse type '%s'\n", optarg); }
        case '?':
        case 'h':
        default:
            return syntax(opt, argv[0]);
        }
    }

    std::string filename;
    if (optind >= argc) {
        char dir[500];
        if (!getcwd(dir, 500))
            return 1;
        filename = dir + std::string("/.rtags.db");
    } else {
        filename = argv[optind];
    }

    FileDB db;
    if (db.open(filename.c_str(), Database::ReadOnly)) {
        Database::iterator *it = db.createIterator(Database::All);
        if (it->isValid()) {
            do {
                printf("%s:%d\n", it->key().constData(), it->value().size());
            } while (it->next());
            delete it;
        }
    }
    
    // if (createExpect)
    //     return writeExpect(filename) ? 0 : 2;
    // else
    //     dumpDatabase(filename, type);

    return 0;
}
