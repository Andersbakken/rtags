#include "RBuild.h"
#include "Path.h"
#include "Precompile.h"
#include "Mmap.h"
#include <QCoreApplication>
#include <QDir>
#include <getopt.h>
#include <RTags.h>
#include <AtomicString.h>

static inline void usage(const char* argv0, FILE *f)
{
    fprintf(f,
            "%s [options]...\n"
            "  --help|-h                  Display this help\n"
            "  --db-file|-d [arg]         Use this database file\n"
            "  --update|-u                Update database\n"
            "  --source-dir|-s [arg]      Recurse this directory\n"
            "  --dont-clang|-c            Don't actually do much of anything\n"
            "  --db-type|-t [arg]         Type of db (leveldb or filedb)\n",
            argv0);
}

using namespace RTags;

class PrecompileScope
{
public:
    PrecompileScope() {}
    ~PrecompileScope() { Precompile::cleanup(); }
};

int main(int argc, char** argv)
{
    QCoreApplication::setOrganizationName("RTags");
    QCoreApplication::setOrganizationDomain("https://github.com/Andersbakken/rtags");
    QCoreApplication::setApplicationName("RTags");
    QCoreApplication app(argc, argv);
    Path db;
    Path srcDir;
    bool update = false;
    unsigned flags = 0;

    Mmap::init();

    PrecompileScope prescope;

    struct option longOptions[] = {
        { "help", 0, 0, 'h' },
        { "db-file", 1, 0, 'd' },
        { "update", 0, 0, 'u' },
        { "source-dir", 1, 0, 's' },
        { "db-type", 1, 0, 't' },
        { "dont-clang", 0, 0, 'c' },
        { 0, 0, 0, 0 },
    };
    const char *shortOptions = "hud:s:t:Dc";

    int idx, longIndex;
    while ((idx = getopt_long(argc, argv, shortOptions, longOptions, &longIndex)) != -1) {
        switch (idx) {
        case '?':
            usage(argv[0], stderr);
            return 1;
        case 'c':
            flags |= RBuild::ClangDisabled;
            break;
        case 's':
            srcDir = optarg;
            break;
        case 'h':
            usage(argv[0], stdout);
            return 0;
        case 'u':
            update = true;
            break;
        case 'd':
            db = QByteArray(optarg);
            break;
        case 't':
            setenv("RTAGS_DB_TYPE", optarg, 1);
            break;
        default:
            printf("%s\n", optarg);
            break;
        }
    }

    if (update && !srcDir.isEmpty()) {
        fprintf(stderr, "Can't use --source-dir with --update");
        return 1;

    }
    if (db.isEmpty()) {
        if (update) {
            db = findRtagsDb();
        } else {
            db = ".rtags.db";
        }
    }
    if (update && !db.exists()) {
        fprintf(stderr, "No db dir, exiting\n");
        return 1;
    }

    RBuild build(flags);
    build.setDBPath(db);
    if (update) {
        build.updateDB();
        return 0;
    } else {
        bool ok;
        Path appPath = Path::resolved(QDir::currentPath().toLocal8Bit(), Path(), &ok);
        if (!ok)
            qFatal("Unable to resolve initial path");
        const char *makefile = "Makefile";
        if (optind < argc)
            makefile = argv[optind];
        Path p = Path::resolved(makefile, appPath);
        if (p.isDir())
            p += "/Makefile";
        if (!build.buildDB(p, srcDir))
            return 1;
        return app.exec();
    }
}
