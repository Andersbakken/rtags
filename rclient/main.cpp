#include <sstream>
#include <leveldb/db.h>
#include <stdio.h>
#include <unistd.h>

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

int main(int argc, char** argv)
{
    std::string filename;
    std::string key;
    if (argc < 3)
        return 1;

    char dir[500];
    if (!getcwd(dir, 500))
        return 1;
    filename = dir + std::string("/.rtags.db");
    key = argv[2];

    std::string val = fetchValue(filename, key);
    if (!val.empty())
        printf("%s\n", val.c_str());
    return 0;
}
