#include <sstream>
#include <leveldb/db.h>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <RTags.h>
#include <QtCore>
#include <GccArguments.h>

using namespace RTags;

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

static inline std::string symbolNameAt(const std::string &location)
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

enum Type { Symbol = 0x01, Reference = 0x02, Dependency = 0x04, Dict = 0x08, All = 0x0f };

static inline void dumpDatabase(const std::string& filename, int type)
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
        const std::string key = it->key().ToString();
        std::string fileName;
        unsigned line, col;
        // qDebug() << key.c_str();
        if (parseLocation(key, fileName, line, col)) {
            leveldb::Slice val = it->value();
            const QByteArray v = QByteArray::fromRawData(val.data(), val.size());
            QByteArray mapsTo;
            QSet<QByteArray> references;
            QDataStream ds(v);
            ds >> mapsTo >> references;
            // printf("%s (%s) maps to %s (%s)\n",
            //        key.c_str(),
            //        symbolNameAt(key).c_str(),
            //        it->value().ToString().c_str(),
            //        symbolNameAt(it->value().ToString()).c_str());
            if (type & Symbol) {
                printf("%s maps to %s\n",
                       key.c_str(),
                       mapsTo.constData());
            }
            if (type & Reference) {
                // printf("Balle %d %d %d %d\n", references.size(), v.size(), val.size(),
                //        it->value().ToString().size());
                foreach(const QByteArray &r, references) {
                    printf("%s refers to %s\n", r.constData(), key.c_str());
                }
            }
        } else if (key.substr(0, 2) == "d:") { // dict
            if (type & Dict) {
                QByteArray val(it->value().data(), it->value().size());
                if (val.endsWith('\0'))
                    val.chop(1);
                val.replace('\0', " :: ");
                printf("dict entry %s: %s\n", key.substr(2).c_str(), val.constData());
            }
        } else if (key.substr(0, 2) == "f:") { // dependency
            if (type & Dependency) {
                const QByteArray ba = QByteArray::fromRawData(it->value().data(),
                                                              it->value().size());
                QDataStream ds(ba);
                GccArguments args;
                ds >> args;
                time_t lastModified;
                ds >> lastModified;
                printf("%s %s %s", key.c_str() + 2, args.raw().constData(), ctime(&lastModified));
                QHash<QByteArray, time_t> dependencies;
                ds >> dependencies;
                for (QHash<QByteArray, time_t>::const_iterator it = dependencies.begin();
                     it != dependencies.end(); ++it) {
                    printf("  %s %s", it.key().constData(), ctime(&it.value()));
                }
            }
        }
    }
    delete it;
    delete db;
}

static inline std::string removePath(const std::string& line)
{
    std::string::size_type slash = line.rfind('/');
    if (slash == std::string::npos)
        return line;
    return line.substr(slash + 1);
}

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
            fprintf(f, "--follow-symbol\n%s\n%s\n\n",
                    removePath(key).c_str(),
                    removePath(it->value().ToString()).c_str());
        }
    }

    fclose(f);

    delete it;
    delete db;

    printf("expect.txt written\n");

    return true;
}

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
        case 'i':
            *type |= Dict;
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
    } else
        filename = argv[optind];

    if (createExpect)
        return writeExpect(filename) ? 0 : 2;
    else
        dumpDatabase(filename, type);

    return 0;
}
