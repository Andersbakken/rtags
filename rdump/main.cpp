#include <sstream>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <RTags.h>
#include <QtCore>
#include <GccArguments.h>
#include "Database.h"

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

    QByteArray filename;
    if (optind >= argc) {
        filename = findRtagsDb();
    } else {
        filename = argv[optind];
    }

    Database* db = Database::create(filename, Database::ReadOnly);
    if (db->isOpened()) {
        const char *names[] = { "General", "Dictionary", "References", "Targets" };
        for (int i=0; i<Database::NumConnectionTypes; ++i) {
            Database::iterator *it = db->createIterator(static_cast<Database::ConnectionType>(i));
            if (it->isValid()) {
                do {
                    printf("%s '%s' => %d bytes", names[i], it->key().constData(), it->value().size());
                    if (it->value().size() == 4) {
                        const QByteArray ba = it->value();
                        QDataStream ds(ba);
                        int t;
                        ds >> t;
                        printf(" (%d)\n", t);
                    } else {
                        switch (i) {
                        case Database::Targets: {
                            Location loc = it->value<Location>();
                            printf(" (%d:%d:%d)", loc.file, loc.line, loc.column);
                            break; }
                        default:
                            break;
                        }
                        printf("\n");
                    }
                } while (it->next());
            }
            delete it;
        }
    }
    
    // if (createExpect)
    //     return writeExpect(filename) ? 0 : 2;
    // else
    //     dumpDatabase(filename, type);

    delete db;
    return 0;
}
