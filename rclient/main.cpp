#include <sstream>
#include <leveldb/db.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

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
    std::string key = argv[2];

    std::string val = fetchValue(filename, key);
    if (!val.empty())
        printf("%s\n", val.c_str());
    return 0;
}
