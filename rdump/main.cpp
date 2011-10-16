#include <sstream>
#include <leveldb/db.h>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <Shared.h>

static inline int readLine(FILE *f, char *buf, int max)
{
    assert(!buf == (max == -1));
    if (max == -1)
        max = INT_MAX;
    for (int i=0; i<max; ++i) {
        const int ch = fgetc(f);
        if (ch == '\n' || ch == EOF)
            return i;
        if (buf)
            *buf++ = *reinterpret_cast<const char*>(&ch);
    }
    return -1;
}

std::string symbolAt(const std::string &location)
{
    std::string fileName, ret;
    unsigned line = 0, col = 0;
    if (parseLocation(location, fileName, line, col)) {
        FILE *f = fopen(fileName.c_str(), "r");
        if (f) {
            for (unsigned i=0; i<line - 1; ++i)
                readLine(f, 0, -1);
            char line[1024] = { 0 };
            readLine(f, line, 1024);
            // ### strip out parts that likely aren't part of the symbol
            ret = line + (col - 1);
            fclose(f);
        }
    }
    return ret;
}
void dumpDatabase(const std::string& filename)
{
    leveldb::DB* db;
    if (!leveldb::DB::Open(leveldb::Options(), filename, &db).ok()) {
        fprintf(stderr, "Unable to open db %s\n", filename.c_str());
        return;
    }
    leveldb::Iterator* it = db->NewIterator(leveldb::ReadOptions());
    for (it->SeekToFirst(); it->Valid(); it->Next()) {
        // unsigned line = 0, col = 0;;
        // std::string mod = it->key().ToString();
        // printf("%d\n", parseLocation(it->key().ToString(), mod, line, col));
        // printf("%s %d %d\n", mod.c_str(), line, col);
        printf("%s (%s) maps to %s (%s)\n",
               it->key().ToString().c_str(),
               symbolAt(it->key().ToString()).c_str(),
               it->value().ToString().c_str(),
               symbolAt(it->value().ToString()).c_str());
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
