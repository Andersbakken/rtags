#include <sstream>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <RTags.h>
#include <QtCore>
#include <GccArguments.h>
#include <Source.h>
#include "Database.h"
#include "Mmap.h"

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

static Location locationFromKey(const QByteArray &key)
{
    Location loc;
    unsigned *uints[] = { &loc.file, &loc.line, &loc.column };
    const QList<QByteArray> parts = key.split(':');
    for (int i=0; i<qMin(3, parts.size()); ++i) {
        *uints[i] = parts.at(i).toUInt();
    }
    return loc;
}

int main(int argc, char** argv)
{
    Mmap::init();

    bool createExpect = false;
    int opt, type = All;
    char newLine = '\n';
    while ((opt = getopt(argc, argv, "eht:n")) != -1) {
        switch (opt) {
        case 'e':
            createExpect = true;
            break;
        case 'n':
            newLine = ' ';
            break;
        case 't':
            if (parseType(optarg, &type))
                break;
            fprintf(stderr, "Unable to parse type '%s'\n", optarg); 
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
                    const QByteArray key(it->key().constData(), it->key().size());
                    printf("%s '%s' => %d bytes", names[i], key.constData(), it->value().size());
                    if (it->value().size() == 4) {
                        const QByteArray ba = it->value();
                        QDataStream ds(ba);
                        int t;
                        ds >> t;
                        printf(" (%d)%c", t, newLine);
                    } else {
                        switch (i) {
                        case Database::Targets:
                            printf(" (%s => %s)%c",
                                   db->locationToString(locationFromKey(key)).constData(),
                                   db->locationToString(it->value<Location>()).constData(), newLine);
                            break;
                        case Database::General:
                            if (key == "files") {
                                printf("%c", newLine);
                                foreach(const Path &file, it->value<QSet<Path> >()) {
                                    printf("    %s%c", file.constData(), newLine);
                                }
                            } else if (key == "filesByName") {
                                printf("%c", newLine);
                                const QHash<Path, int> filesToIndex = it->value<QHash<Path, int> >();
                                for (QHash<Path, int>::const_iterator it = filesToIndex.begin();
                                     it != filesToIndex.end(); ++it) {
                                    printf("    %s (id: %d)%c", key.constData(), it.value(), newLine);
                                }
                            } else if (key == "sourceDir") {
                                printf(" (%s)%c", it->value<Path>().constData(), newLine);
                            } else if (key == "sources") {
                                printf("%c", newLine);
                                foreach(const Source &src, it->value<QList<Source> >()) {
                                    printf("    %s (%s)", src.path.constData(),
                                           qPrintable(QDateTime::fromTime_t(src.lastModified).toString()));
                                    foreach(const QByteArray &arg, src.args) {
                                        printf(" %s", arg.constData());
                                    }
                                    printf("%cDependencies:%c", newLine, newLine);
                                    for (QHash<Path, quint64>::const_iterator it = src.dependencies.begin();
                                         it != src.dependencies.end(); ++it) {
                                        printf("      %s (%s)%c", it.key().constData(),
                                               qPrintable(QDateTime::fromTime_t(it.value()).toString()), newLine);
                                    }
                                }
                            } else {
                                fprintf(stderr, "Unknown key General: [%s]\n", key.constData());
                            }
                            break;
                        case Database::Dictionary:
                            printf("%c", newLine);
                            foreach(const DictionaryEntry &entry, it->value<QSet<DictionaryEntry> >()) {
                                printf("    ");
                                for (int i=0; i<entry.scope.size(); ++i) {
                                    printf("%s::", entry.scope.at(i).constData());
                                }
                                printf("%s ", key.constData());
                                bool first = true;
                                foreach(const Location &l, entry.locations) {
                                    if (!first) {
                                        printf(", ");
                                    } else {
                                        first = false;
                                    }
                                    printf("%s", db->locationToString(l).constData());
                                }
                                printf("%c", newLine);
                            }
                            break;
                        case Database::References:
                            printf("%c", newLine);
                            foreach(const Location &l, it->value<QSet<Location> >())
                                printf("    %s%c", db->locationToString(l).constData(), newLine);
                            break;
                        default:
                            printf("\n");
                            break;
                        }
                        if (newLine != '\n')
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
