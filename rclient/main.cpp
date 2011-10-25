#include <sstream>
#include <leveldb/db.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <QtCore>

// std::string fetchValue(const std::string& filename, const std::string& key)
// {
//     leveldb::DB* db;
//     if (!leveldb::DB::Open(leveldb::Options(), filename, &db).ok()) {
//         fprintf(stderr, "Unable to open db %s\n", filename.c_str());
//         return std::string();
//     }
//     std::string value;
//     db->Get(leveldb::ReadOptions(), key, &value);
//     delete db;
//     return value;
// }

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


int main(int argc, char** argv)
{
    if (argc < 3)
        return 1;

    std::string filename = findRtagsDb("/.rtags.db");
    if (filename.empty())
        return 2;
    leveldb::DB* db;
    if (!leveldb::DB::Open(leveldb::Options(), filename, &db).ok()) {
        fprintf(stderr, "Unable to open db %s\n", filename.c_str());
        return 1;
    }
    Scope scope(db);
   
    std::string cmd = argv[1];
    std::string key = argv[2];
    if (cmd == "--follow-symbol" || cmd == "-f") {
        if (!followSymbol(db, key))
            maybeDict(db, key, followSymbol);
    } else if (cmd == "--find-references" || cmd == "-r") {
        if (!findReferences(db, key))
            maybeDict(db, key, findReferences);
    } else if (cmd == "--symbol-names" || cmd == "-S") {
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
            if (key.empty() || strstr(k.c_str(), key.c_str())) {
                printf("%s\n", k.c_str() + 2);
            }
            it->Next();
        }
        delete it;
    }

    return 0;
}
