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

static inline void raw(Database *db, const QByteArray &file)
{
    const char *names[] = { "General: [", "Dictionary: [", "References: [", "Targets: [" };
    QFile f(file);
    if (!f.open(QIODevice::WriteOnly)) {
        fprintf(stderr, "Can't open %s for writing\n", file.constData());
        return;
    }
    QMap<QByteArray, QByteArray> entries[Database::NumConnectionTypes];
    for (int i=0; i<Database::NumConnectionTypes; ++i) {
        Database::iterator *it = db->createIterator(static_cast<Database::ConnectionType>(i));
        if (it->isValid()) {
            do {
                //Q_ASSERT_X(!entries[i].contains(it->key()), "raw()", (it->key() + " already exists in " + names[i]).constData());
                if (entries[i].contains(it->key()))
                    qDebug() << "already contains key" << it->key() << names[i];
                entries[i][it->key()] = it->value();
            } while (it->next());
        }
        delete it;
    }
    for (int i = 0; i < Database::NumConnectionTypes; ++i) {
        const QMap<QByteArray, QByteArray>& cat = entries[i];
        QMap<QByteArray, QByteArray>::const_iterator d = cat.begin();
        QMap<QByteArray, QByteArray>::const_iterator dend = cat.end();
        while (d != dend) {
            f.write(names[i]);
            f.write(d.key());
            f.write("] [");
            const QByteArray value = d.value();
            char buf[16];
            for (int i=0; i<value.size(); ++i) {
                char *b = buf;
                if (i > 0) {
                    buf[0] = ',';
                    buf[1] = ' ';
                    b = buf + 2;
                }
                const int ret = snprintf(b, 15, "0x%x", value.at(i));
                f.write(buf, ret + (b - buf));
            }
            f.write("]\n");
            ++d;
        }
    }
}

int main(int argc, char** argv)
{
    Mmap::init();

    int opt;
    char newLine = '\n';
    QByteArray rawFile;
    while ((opt = getopt(argc, argv, "hnr:")) != -1) {
        switch (opt) {
        case 'n':
            newLine = ' ';
            break;
        case 'r':
            rawFile = optarg;
            break;
        case '?':
        case 'h':
        default:
            fprintf(stderr, "rdump -[rn]\n");
            return 0;
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
        if (!rawFile.isEmpty()) {
            raw(db, rawFile);
            delete db;
            return 0;
        }
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
                                    printf("    %s (id: %d)%c", it.key().constData(), it.value(), newLine);
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
                        case Database::Dictionary: {
                            printf("%c", newLine);
                            const DictionaryHash &dh = it->value<DictionaryHash>();
                            for (DictionaryHash::const_iterator hit = dh.begin(); hit != dh.end(); ++hit) {
                                printf("    ");
                                const QList<QByteArray> &scope = hit.key();
                                for (int i=0; i<scope.size(); ++i) {
                                    printf("%s::", scope.at(i).constData());
                                }
                                printf("%s ", key.constData());
                                bool first = true;
                                foreach(const Location &l, hit.value()) {
                                    if (!first) {
                                        printf(", ");
                                    } else {
                                        first = false;
                                    }
                                    printf("%s", db->locationToString(l).constData());
                                }
                                printf("%c", newLine);
                            }
                            break; }
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
