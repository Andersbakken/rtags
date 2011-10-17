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
        if (!val.empty() && strlen(val.c_str()))
            printf("%s\n", val.c_str());
    } else if (cmd == "--find-references" || cmd == "-r") {
        std::string val;
        db->Get(leveldb::ReadOptions(), key, &val);
        if (!val.empty()) {
            const char *refPtr = val.c_str() + strlen(val.c_str()) + 1;
            std::string refs;
            db->Get(leveldb::ReadOptions(), refPtr, &refs);
            if (refs.size()) {
                QByteArray data = QByteArray::fromRawData(refs.c_str(), refs.size());
                QDataStream ds(data);
                int num;
                QByteArray entry;
                ds >> num;
                for (int i = 0; i < num; ++i) {
                    ds >> entry;
                    if (!entry.isEmpty()) {
                        printf("%s\n", entry.constData());
                    } else {
                        printf("%s %d: } else {\n", __FILE__, __LINE__);
                    }
                }
            }
        }
    }
    return 0;
}
