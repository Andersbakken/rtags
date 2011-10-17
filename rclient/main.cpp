#include <sstream>
#include <leveldb/db.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <QtCore>

std::string fetchValue(const std::string& filename, const std::string& key)
{
    leveldb::DB* db;
    if (!leveldb::DB::Open(leveldb::Options(), filename, &db).ok()) {
        fprintf(stderr, "Unable to open db %s\n", filename.c_str());
        return std::string();
    }
    std::string value;
    db->Get(leveldb::ReadOptions(), key, &value);
    delete db;
    return value;
}

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

int main(int argc, char** argv)
{
    if (argc < 3)
        return 1;

    std::string filename = findRtagsDb("/.rtags.db");
    if (filename.empty())
        return 2;
    std::string cmd = argv[1];
    std::string key = argv[2];
    if (cmd == "--follow-symbol") {
        std::string val = fetchValue(filename, key);
        if (!val.empty())
            printf("%s\n", val.c_str());
    } else if (cmd == "--find-references" || cmd == "-r") {
        std::string val = fetchValue(filename, key);
        if (!val.empty()) {
            const char *refPtr = val.c_str() + strlen(val.c_str()) + 1;
            char buf[100];
            snprintf(buf, 100, "ref:%s", refPtr);
            const leveldb::Slice refs = fetchValue(filename, buf);
            QByteArray data = QByteArray::fromRawData(refs.data(), refs.size());
            QDataStream ds(data);
            int num;
            char *entry;
            ds >> num;
            Q_ASSERT(num >= 0);
            // if (num > 0)
            //     printf("refs for %s\n", key.substr(4).c_str());
            for (int i = 0; i < num; ++i) {
                ds >> entry;
                printf("%s\n", entry);
                delete[] entry;
            }
            // if (num > 0)
            //     printf("---\n");
            // printf("%s\n", refPtr);
        }
    }
    return 0;
}
