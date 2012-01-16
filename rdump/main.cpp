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

static inline void validateExpect(Database *db)
{
    QFile f("expect.txt");
    qDebug() << QDir::current();
    if (!f.open(QIODevice::ReadOnly)) {
        printf("%s:%d if (!f.open(QIODevice::ReadOnly))\n", __FILE__, __LINE__);
        return;
    }
    while (!f.atEnd()) {
        const QByteArray line = f.readLine();
        int idx = line.indexOf(" => ");
        if (idx == -1) {
            qWarning("Couldn't parse line '%s'", line.constData());
            continue;
        }
        qDebug() << "got line" << line;
    }
}

static inline void writeExpect(Database *db)
{
    Q_ASSERT(db);
    QFile f("expect.txt");
    if (!f.open(QIODevice::WriteOnly)) {
        fprintf(stderr, "Can't open expect.txt for writing\n");
        return;
    }
    QFile ff("expect.b");
    if (!ff.open(QIODevice::WriteOnly)) {
        fprintf(stderr, "Can't open expect.txt for writing\n");
        return;
    }
    
    char buf[512];
    const bool cwd = getcwd(buf, 512);
    Q_ASSERT(cwd);
    (void)cwd;
    QByteArray path = buf;
    if (!path.endsWith("/"))
        path.append("/");
    
    Database::iterator *it = db->createIterator(Database::Targets);
    if (it->isValid()) {
        do {
            const QByteArray s = db->locationToString(Location::fromKey(it->key()),
                                                      Database::RelativeToRoot);
            const QByteArray target = db->locationToString(it->value<Location>(),
                                                           Database::RelativeToRoot);
            f.write("rc --sort-output --separate-paths-by-space --no-context --paths-relative-to-root --follow-symbol ");
            f.write(s);
            f.write(" => ");
            f.write(target);
            f.putChar('\n');
        } while (it->next());
    }
    delete it;
    it = db->createIterator(Database::References);
    if (it->isValid()) {
        do {
            if (it->key().endsWith(':')) {
                Location loc = Location::fromKey(it->key());
                QSet<Location> refs = db->findReferences(loc);
                const QByteArray src = db->locationToString(loc, Database::RelativeToRoot);
                f.write("rc --sort-output --separate-paths-by-space --no-context --paths-relative-to-root --find-references ");
                f.write(src);
                f.write(" => ");
                QList<QByteArray> references;
                foreach(const Location &ref, refs) {
                    references.append(db->locationToString(ref, Database::RelativeToRoot));
                }
                qSort(references);
                foreach(const QByteArray &r, references) {
                    f.write(r);
                    f.putChar(' ');
                }
                f.putChar('\n');
                continue;
            }
        } while (it->next());
    }
    delete it;
    foreach(const QByteArray &symbol, db->listSymbols()) {
        f.write("rc --sort-output --separate-paths-by-space --no-context --paths-relative-to-root --find-symbol '");
        f.write(symbol);
        f.write("' => ");
        QList<QByteArray> locations;
        foreach(const Location &ref, db->findSymbol(symbol)) {
            locations.append(db->locationToString(ref, Database::RelativeToRoot));
        }
        qSort(locations);
        foreach(const QByteArray &l, locations) {
            f.write(l);
            f.putChar(' ');
        }
        f.putChar('\n');
    }
    foreach(const Location &loc, db->allLocations()) {
        f.write("rc --sort-output --separate-paths-by-space --no-context --paths-relative-to-root --all-references ");
        f.write(db->locationToString(loc, Database::RelativeToRoot));
        f.write(" => ");

        QList<QByteArray> out;
        foreach(const Location &l, db->allReferences(loc)) {
            out.append(db->locationToString(l, Database::RelativeToRoot));
        }

        qSort(out);
        foreach(const QByteArray &o, out) {
            f.write(o);
            f.putChar(' ');
        }
        f.putChar('\n');
    }

    printf("Wrote expect.txt\n");
}

static inline void raw(Database *db, const QByteArray &file)
{
    const char *names[] = { "General: [", "Dictionary: [", "References: [", "Targets: [", "ExtraDeclarations: [" };
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
    enum Mode {
        Normal,
        Expect,
        Raw,
        Validate
    } mode = Normal;
    while ((opt = getopt(argc, argv, "hnr:ev")) != -1) {
        switch (opt) {
        case 'n':
            newLine = ' ';
            break;
        case 'r':
            mode = Raw;
            rawFile = optarg;
            break;
        case 'e':
            mode = Expect;
            break;
        case 'v':
            mode = Validate;
            break;
        case '?':
        case 'h':
        default:
            fprintf(stderr, "rdump -[rnev]\n");
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
        switch (mode) {
        case Expect:
            writeExpect(db);
            return 0;
        case Raw:
            raw(db, rawFile);
            delete db;
            return 0;
        case Validate:
            validateExpect(db);
            delete db;
            return 0;
        case Normal:
            break;
        }
        const char *names[] = { "General", "Dictionary", "References", "Targets", "ExtraDeclarations" };
        for (int i=0; i<Database::NumConnectionTypes; ++i) {
            Database::iterator *it = db->createIterator(static_cast<Database::ConnectionType>(i));
            if (it->isValid()) {
                do {
                    const QByteArray key(it->key().constData(), it->key().size());
                    QByteArray coolKey;
                    if (i == Database::Targets || (i == Database::References && key.endsWith(":"))
                        || i == Database::ExtraDeclarations) {
                        coolKey = db->locationToString(locationFromKey(it->key())) + ' ';
                    }
                    printf("%s '%s' %s=> %d bytes", names[i], key.constData(), coolKey.constData(), it->value().size());
                    if (it->value().size() == 4) {
                        int t = it->value<int>();
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
                        case Database::ExtraDeclarations:
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
        printf("%d entries\n", db->count());
    }

    // if (createExpect)
    //     return writeExpect(filename) ? 0 : 2;
    // else
    //     dumpDatabase(filename, type);

    delete db;
    return 0;
}
