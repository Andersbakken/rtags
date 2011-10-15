#include <sstream>
#include <leveldb/db.h>
#include <stdio.h>
#include <unistd.h>

void dumpDatabase(const std::string& filename)
{
    leveldb::DB* db;
    if (!leveldb::DB::Open(leveldb::Options(), filename, &db).ok()) {
        fprintf(stderr, "Unable to open db %s\n", filename.c_str());
        return;
    }
    leveldb::Iterator* it = db->NewIterator(leveldb::ReadOptions());
    for (it->SeekToFirst(); it->Valid(); it->Next()) {
        printf("%s maps to %s\n", it->key().ToString().c_str(), it->value().ToString().c_str());
    }
    delete it;
    delete db;
}

int main(int argc, char** argv)
{
    std::string filename;
    if (argc < 2) {
        char dir[500];
        if (!getcwd(dir, 500))
            return 1;
        filename = dir + std::string("/.rtags.db");
    } else
        filename = argv[1];
    dumpDatabase(filename);
    return 0;
}
