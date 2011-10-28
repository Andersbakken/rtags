#include "RBuild.h"
#include "Path.h"
#include "Precompile.h"
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
            "  --update|-u                Update database\n",
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
    // {
    //     AtomicString *foo = new AtomicString("bar");
    //     AtomicString bar = "bar";
    //     bar.clear();
    //     delete foo;
    //     // bar = foo;
    //     // // bar = "123";
    //     // // bar = "bar";
    //     // // foo = bar;
    //     // foo.clear();
    //     // bar.clear();
    // }
    // return 0;
    QCoreApplication app(argc, argv);
    Path db;
    bool update = false;

    PrecompileScope prescope;

    struct option longOptions[] = {
        { "help", 0, 0, 'h' },
        { "db-file", 1, 0, 'd' },
        { "update", 0, 0, 'u' },
        { 0, 0, 0, 0 },
    };
    const char *shortOptions = "hud:";

    int idx, longIndex;
    while ((idx = getopt_long(argc, argv, shortOptions, longOptions, &longIndex)) != -1) {
        switch (idx) {
        case '?':
            usage(argv[0], stderr);
            return 1;
        case 'h':
            usage(argv[0], stdout);
            return 0;
        case 'u':
            update = true;
            break;
        case 'd':
            db = QByteArray(optarg);
            break;
        default:
            printf("%s\n", optarg);
            break;
        }
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

    RBuild build;
    build.setDBPath(db);
    if (update) {
        build.updateDB();
        return 0;
    } else {
        bool ok;
        Path appPath = Path::resolved(QDir::currentPath().toLocal8Bit(), Path(), &ok);
        if (!ok)
            qFatal("Unable to resolve initial path");
        build.buildDB(Path::resolved("Makefile", appPath));
        return app.exec();
    }
}
