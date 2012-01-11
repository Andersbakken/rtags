#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <QtCore>
#include <getopt.h>
#include <RTags.h>
#include <Location.h>
#include <AtomicString.h>
#include "Database.h"
#include "Mmap.h"

using namespace RTags;

class Output
{
public:
    enum Flag {
        PathsRelativeToRoot = 0x01,
        NoContext = 0x02,
        SeparateLocationsBySpace = 0x08,
        PrecedingSpacePending = 0x10,
        SortOutput = 0x20
    };

    Output(unsigned flags)
        : mFlags(flags)
    {}

    unsigned flags() const
    {
        return mFlags;
    }

    QHash<const Database*, QByteArray> usedDatabases() const
    {
        return mUsedDBs;
    }
    void printLocation(const Location &loc, const Database *db)
    {
        printf("%s\n", printLocationImpl(loc, db, mFlags & ~SeparateLocationsBySpace).constData());
    }

    template <typename T> void printLocations(const T &t, const Database *db)
    {
        unsigned flags = mFlags;
        QList<QByteArray> out;
        foreach(const Location &l, t)
            out.append(printLocationImpl(l, db, flags));
        if (flags & SortOutput)
            qSort(out);
        const int count = out.size();
        for (int i=0; i<count; ++i) {
            if ((flags & SeparateLocationsBySpace)) {
                if (i) {
                    printf(" %s", out.at(i).constData());
                } else {
                    printf("%s", out.at(i).constData());
                }
            } else {
                printf("%s\n", out.at(i).constData());
            }
        }
        if (flags & SeparateLocationsBySpace)
            printf("\n");
    }

    void print(QList<QByteArray> out)
    {
        if (SortOutput)
            qSort(out);
        foreach(const QByteArray &line, out) {
            printf("%s\n", line.constData());
        }
    }

private:
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

    inline QByteArray printLocationImpl(const Location &loc, const Database *db, unsigned flags)
    {
        Q_ASSERT(loc.file);
        QByteArray out = db->locationToString(loc, (flags & PathsRelativeToRoot
                                                    ? Database::RelativeToRoot
                                                    : Database::None));
        if (flags & PrecedingSpacePending)
            out.prepend(' ');

        QByteArray &ref = mUsedDBs[db];
        if (ref.isEmpty())
            ref = db->path();

        if (!(flags & NoContext)) {
            FILE *f = fopen(db->path(loc).constData(), "r");
            if (f) {
                Q_ASSERT(loc.line > 0);
                for (unsigned i=0; i<loc.line - 1; ++i)
                    readLine(f, 0, -1);
                char buf[1024] = { 0 };
                readLine(f, buf, 1024);
                fclose(f);
                out += '\t';
                out += buf;
            }
        }
        return out;
    }
    QHash<const Database*, QByteArray> mUsedDBs;
    unsigned mFlags;
};

static inline void usage(const char* argv0, FILE *f)
{
    fprintf(f,
            "%s [options]...\n"
            "  --help|-h                     Display this help\n"
            "  --db-file|-d [arg]            Find database using this path\n"
            "  --print-db-path|-p            Print out the used database path(s)\n"
            "  --detect-db|-D                Find .rtags.db based on path\n"
            "                                (default when no -d options are specified)\n"
            "  --db-type|-t [arg]            Type of db (leveldb or filedb)\n"
            "  --paths-relative-to-root|-n   Print out files matching arg\n"
            "  --no-context|-N               Don't print context from files when printing locations\n"
            "  --separate-paths-by-space|-S  Separate multiple locations by space instead of newline\n"
            "  --sort-output|-o              Sort output alphabetically\n"
            "\n"
            "  Modes\n"
            "  --follow-symbol|-f [arg]      Follow this symbol (e.g. /tmp/main.cpp:32:1)\n"
            "  --find-references|-r [arg]    Print references of symbol at arg\n"
            "  --list-symbols|-l [arg]       Print out symbols names matching arg\n"
            "  --files|-P [arg]              Print out files matching arg\n"
            "  --all-references|-a [arg]     Print all references/declarations/definitions that matches arg\n"
            "  --find-symbols|-s [arg]       Print out symbols matching arg\n",
            argv0);
}

int main(int argc, char** argv)
{
    {
        QFile log("/tmp/rc.log");
        log.open(QIODevice::Append);
        char buf[512];
        const bool cwd = getcwd(buf, 512);
        if (cwd) {
            log.write("( cd ");
            log.write(buf);
            log.write(" && ");
        }

        for (int i=0; i<argc; ++i) {
            log.putChar('\'');
            log.write(argv[i]);
            log.putChar('\'');
            log.putChar(' ');
        }
        if (cwd)
            log.write(" )");
        log.putChar('\n');
    }

    // ### print db path
    struct option longOptions[] = {
        { "help", no_argument, 0, 'h' },
        { "follow-symbol", required_argument, 0, 'f' },
        { "db", required_argument, 0, 'd' },
        { "find-references", required_argument, 0, 'r' },
        { "find-symbols", required_argument, 0, 's' },
        { "find-db", no_argument, 0, 'D' },
        { "list-symbols", optional_argument, 0, 'l' },
        { "files", optional_argument, 0, 'P' },
        { "paths-relative-to-root", no_argument, 0, 'n' },
        { "db-type", required_argument, 0, 't' },
        { "all-references", required_argument, 0, 'a' },
        { "no-context", no_argument, 0, 'N' },
        { "separate-paths-by-space", no_argument, 0, 'S' },
        { "sort-output", no_argument, 0, 'o' },
        { 0, 0, 0, 0 },
    };
    const char *shortOptions = "hf:d:r:l::Dps:P::nt:a:NSo";

    Mmap::init();

    QList<QByteArray> dbPaths;

    enum Mode {
        None,
        FollowSymbol,
        References,
        FindSymbols,
        ListSymbols,
        Files,
        AllReferences
        // RecursiveReferences,
    } mode = None;
    unsigned flags = 0;
    int idx, longIndex;
    QByteArray arg;
    opterr = 0;
    while ((idx = getopt_long(argc, argv, shortOptions, longOptions, &longIndex)) != -1) {
        switch (idx) {
        case '?':
            usage(argv[0], stderr);
            fprintf(stderr, "rc: invalid option \"%s\"\n", argv[optind]);
            return 1;
        case 'N':
            flags |= Output::NoContext;
            break;
        case 'S':
            flags |= Output::SeparateLocationsBySpace;
            break;
        case 'o':
            flags |= Output::SortOutput;
            break;
        case 'a':
            if (mode != None) {
                fprintf(stderr, "Mode is already set\n");
                return 1;
            }
            mode = AllReferences;
            arg = optarg;
            break;
        case 'n':
            flags |= Output::PathsRelativeToRoot;
            break;
        case 't':
            setenv("RTAGS_DB_TYPE", optarg, 1);
            break;
        case 'h':
            usage(argv[0], stdout);
            return 0;
        case 'f':
            if (mode != None) {
                fprintf(stderr, "Mode is already set\n");
                return 1;
            }
            arg = optarg;
            mode = FollowSymbol;
            break;
        case 'P':
            if (mode != None) {
                fprintf(stderr, "Mode is already set\n");
                return 1;
            }
            if ((!optarg || !strlen(optarg)) && optind < argc && strncmp(argv[optind], "-", 1)) {
                arg = argv[optind++];
            } else {
                arg = optarg;
            }
            mode = Files;
            break;
        case 'r':
            arg = optarg;
            if (mode != None) {
                fprintf(stderr, "Mode is already set\n");
                return 1;
            }
            mode = References;
            break;
        case 'D': {
            const QByteArray db = findRtagsDb();
            if (!db.isEmpty()) {
                dbPaths.append(db);
            }
            break; }
        case 'd':
            if (optarg && strlen(optarg)) {
                const QByteArray db = findRtagsDb(optarg);
                if (!db.isEmpty())
                    dbPaths.append(db);
            }
            break;
        case 'l':
            if (mode != None) {
                fprintf(stderr, "Mode is already set\n");
                return 1;
            }
            if ((!optarg || !strlen(optarg)) && optind < argc && strncmp(argv[optind], "-", 1)) {
                arg = argv[optind++];
            } else {
                arg = optarg;
            }
            mode = ListSymbols;
            break;
        case 's':
            if (mode != None) {
                fprintf(stderr, "Mode is already set\n");
                return 1;
            }
            mode = FindSymbols;
            arg = optarg;
            break;
        }
    }
    if (dbPaths.isEmpty()) {
        QByteArray db = findRtagsDb();
        if (db.isEmpty() && !arg.isEmpty())
            db = findRtagsDb(arg);
        if (!db.isEmpty())
            dbPaths.append(db);
    }

    if (dbPaths.isEmpty()) {
        fprintf(stderr, "No databases specified\n");
        return 1;
    }

    Output output(flags);
    bool done = false;
    foreach(const QByteArray &dbPath, dbPaths) {
        if (dbPath.isEmpty())
            continue;
        Database* db = Database::create(dbPath, Database::ReadOnly);
        if (!db->isOpened()) {
            delete db;
            continue;
        }

        switch (mode) {
        case None:
            usage(argv[0], stderr);
            fprintf(stderr, "No mode selected\n");
            return 1;
        case AllReferences: {
            const Location loc = db->createLocation(arg);
            if (!loc.file) {
                fprintf(stderr, "Invalid arg %s", arg.constData());
                break;
            }
            output.printLocations(db->allLocations(loc), db);
            break; }
        case FollowSymbol: {
            Location loc = db->createLocation(arg);
            // printf("%s => %d:%d:%d\n", arg.constData(), loc.file, loc.line, loc.column);
            if (loc.file) {
                loc = db->followLocation(loc);
                if (loc.file)
                    output.printLocation(loc, db);
                // we're not going to find more than one followLocation
            } else {
                QList<Location> out;
                foreach(const Location &l, db->findSymbol(arg)) {
                    Location ll = db->followLocation(l);
                    if (ll.file)
                        out.append(ll);
                }
                output.printLocations(out, db);
            }
            break; }
        case References: {
            const Location loc = db->createLocation(arg);
            if (loc.file) {
                output.printLocations(db->findReferences(loc), db);
            } else {
                QSet<Location> out;
                foreach(const Location &l, db->findSymbol(arg))
                    out += db->findReferences(l);
                output.printLocations(out, db);
            }
            break; }
        case FindSymbols:
            output.printLocations(db->findSymbol(arg), db);
            break;
        case ListSymbols: {
            const QList<QByteArray> symbolNames = db->listSymbols(arg);
            if (!symbolNames.isEmpty())
                output.print(symbolNames);
            break; }
        case Files: {
            QSet<Path> paths = db->read<QSet<Path> >("files");
            // if (!paths.isEmpty())
            //     sUsedDBs[db] = db->path();
            const bool empty = arg.isEmpty();
            const char *root = "./";
            Path srcDir;
            if (!(flags & Output::PathsRelativeToRoot)) {
                srcDir = db->read<Path>("sourceDir");
                root = srcDir.constData();
            }
            foreach(const Path &path, paths) {
                if (empty || path.contains(arg)) {
                    printf("%s%s\n", root, path.constData());
                }
            }
            break; }
        }
        if (done)
            break;
    }

    return 0;
}
