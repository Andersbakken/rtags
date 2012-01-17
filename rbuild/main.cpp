#include "RBuild.h"
#include "Path.h"
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
            "  --db-file|-b [arg]         Use this database file\n"
            "  --update|-u                Update database\n"
            "  --source-dir|-s [arg]      Recurse this directory\n"
            "  --dont-clang|-c            Don't actually do much of anything\n"
            "  --dont-index|-i            Call clang_indexSourceFile but don't pass any callbacks\n"
            "  --db-type|-t [arg]         Type of db (leveldb or filedb)\n"
            "  --source|-S                Treat input as source file regardless of extension\n"
            "  --includepath|-I [arg]     Add this includepath to all source files\n"
            "  --define|-D [arg]          Add this define to all files\n"
            "  --debug|-d                 Print debug info\n",
            argv0);
}

using namespace RTags;

static inline bool add(QList<Path> &makefiles, QList<Path> &sources, const char *arg,
                       const Path &appPath, bool treatInputAsSourceOverride)
{
    const Path p = Path::resolved(arg, appPath);
    switch (p.type()) {
    case Path::File:
        if (treatInputAsSourceOverride || p.isSource()) {
            sources.append(p);
        } else {
            makefiles.append(p);
        }
        break;
    case Path::Directory: {
        Path makefile = Path::resolved("Makefile", p);
        if (makefile.isFile()) {
            makefiles.append(makefile);
            break;
        }
    }
        // fall through
    default:
        fprintf(stderr, "Invalid input %s\n", arg);
        return false;
    }
    return true;
}

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

    struct option longOptions[] = {
        { "help", no_argument, 0, 'h' },
        { "update", no_argument, 0, 'u' },
        { "db-file", required_argument, 0, 'b' },
        { "source-dir", required_argument, 0, 's' },
        { "db-type", required_argument, 0, 't' },
        { "dont-clang", no_argument, 0, 'c' },
        { "dont-index", no_argument, 0, 'i' },
        { "includepath", required_argument, 0, 'I' },
        { "define", required_argument, 0, 'D' },
        { "source", no_argument, 0, 'S' },
        { "debug", no_argument, 0, 'd' },
        { 0, 0, 0, 0 },
    };
    const QByteArray shortOptions = RTags::shortOptions(longOptions);
    QList<Path> includePaths;
    QList<QByteArray> defines;
    int idx, longIndex;
    bool treatInputAsSourceOverride = false;
    while ((idx = getopt_long(argc, argv, shortOptions.constData(),
                              longOptions, &longIndex)) != -1) {
        switch (idx) {
        case '?':
            usage(argv[0], stderr);
            return 1;
        case 'd':
            flags |= RBuild::DebugAllSymbols;
            break;
        case 'I':
            includePaths += Path::resolved(optarg);
            break;
        case 'D':
            defines += optarg;
            break;
        case 'c':
            flags |= RBuild::DontClang;
            break;
        case 'S':
            treatInputAsSourceOverride = true;
            break;
        case 'i':
            flags |= RBuild::DontIndex;
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
        case 'b':
            db = QByteArray(optarg);
            break;
        case 't':
            setenv("RTAGS_DB_TYPE", optarg, 1);
            break;
        default:
            printf("%c not found\n", idx);
            break;
        }
    }

    if (update && !srcDir.isEmpty()) {
        fprintf(stderr, "Can't use --source-dir with --update");
        return 1;
    }
    if (update && optind < argc) {
        fprintf(stderr, "Can't use --update with inputs");
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
    build.addDefines(defines);
    build.addIncludePaths(includePaths);

    if (update) {
        build.updateDB();
    } else {
        QList<Path> sourceFiles;
        QList<Path> makefiles;
        bool ok;
        const Path appPath = Path::resolved(QDir::currentPath().toLocal8Bit(), Path(), &ok);
        if (!ok)
            qFatal("Unable to resolve initial path");
        
        for (int i=optind; i<argc; ++i) {
            if (!strcmp("-", argv[i])) {
                QFile file;
                if (!file.open(stdin, QIODevice::ReadOnly)) {
                    qWarning("Couldn't open stdin for reading");
                    return 1;
                }
                while (!file.atEnd()) {
                    QByteArray line = file.readLine();
                    if (line.endsWith('\n'))
                        line.chop(1);
                    if (!add(makefiles, sourceFiles, line.constData(), appPath, treatInputAsSourceOverride))
                        return 1;
                }
            } else if (!add(makefiles, sourceFiles, argv[i], appPath, treatInputAsSourceOverride)) {
                return 1;
            }
        }

        if (sourceFiles.isEmpty() && makefiles.isEmpty()) {
            Path makefile = Path::resolved("Makefile", appPath);
            if (makefile.isFile())
                makefiles.append(makefile);
        }
        if (!build.buildDB(makefiles, sourceFiles, srcDir))
            return 1;
    }
    return 0;
}
