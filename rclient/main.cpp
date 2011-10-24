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
        }
    } else if (cmd == "--find-references" || cmd == "-r") {
        std::string val;
        db->Get(leveldb::ReadOptions(), key, &val);
        if (!val.empty()) {
            QByteArray referredTo;
            QSet<QByteArray> references;
            const QByteArray v = QByteArray::fromRawData(val.c_str(), val.size());
            QDataStream ds(v);
            ds >> referredTo >> references;
            foreach(const QByteArray &r, references) {
                printf("%s\n", r.constData());
            }
        }
    }
    return 0;
}
