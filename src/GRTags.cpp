#include "GRTags.h"
#include "Filter.h"
#include "GRParser.h"
#include "Log.h"
#include <getopt.h>
#include <math.h>
#include "leveldb/db.h"

GRTags::GRTags()
{
}

bool GRTags::exec(int argc, char **argv)
{
    option options[] = {
        { "help", no_argument, 0, 'h' },
        { "version", no_argument, 0, 'V' },
        { "verbose", no_argument, 0, 'v' },
        { "dir", required_argument, 0, 'd' },
        { "exclude", required_argument, 0, 'e' },
        { "no-update", no_argument, 0, 'n' },
        { 0, 0, 0, 0 }
    };

    int logLevel = 0;
    Path dir = ".";
    enum Mode {
        Detect,
        Update,
        Create
    } mode = Detect;
    int c;
    while ((c = getopt_long(argc, argv, "hVvd:e:n", options, 0)) != -1) {
        switch (c) {
        case '?':
            return false;
        case 'h':
            printf("grtags [...options]\n"
                   "  --help|-h               Display this help\n"
                   "  --version|-V            Display version information\n"
                   "  --verbose|-v            Increase verbosity\n"
                   "  --exclude|-e [filter]   Exclude this pattern (e.g. .git, *.cpp)\n"
                   "  --dir|-d [directory]    Parse this directory (default .)\n");
            return true;
        case 'V':
            printf("GRTags version 0.1\n");
            return true;
        case 'n':
            mode = Create;
            break;
        case 'e':
            mFilters.append(optarg);
            break;
        case 'v':
            ++logLevel;
            break;
        case 'd':
            dir = optarg;
            if (!dir.isDir()) {
                fprintf(stderr, "%s is not a valid directory\n", optarg);
                return false;
            }
            break;
        }
    }
    Path db;
    dir.resolve();
    if (mode == Detect) {
        Path p = dir;
        while (!p.isEmpty()) {
            db = p + "/.grtags.db";
            if (db.isDir()) {
                mode = Update;
                break;
            }
            db.clear();
            p = p.parentDir();
        }
    }
    if (db.isEmpty())
        db = dir + "/.grtags.db";
    if (mode == Update)
        load(db);
    initLogging(logLevel, Path(), 0);
    dir.visit(&GRTags::visit, this);
    save(db);
    return true;
}

Path::VisitResult GRTags::visit(const Path &path, void *userData)
{
    GRTags *grtags = reinterpret_cast<GRTags*>(userData);
    const Filter::Result result = Filter::filter(path, grtags->mFilters);
    switch (result) {
    case Filter::Filtered:
        warning() << "Filtered out" << path;
        return Path::Continue;
    case Filter::Directory:
        warning() << "Entering directory" << path;
        return Path::Recurse;
    case Filter::File:
        grtags->mFiles[Location::insertFile(path)] = 0;
        break;
    case Filter::Source:
        grtags->parse(path);
        break;
    }
    return Path::Continue;
}

void GRTags::parse(const Path &src)
{
    Timer timer;
    GRParser parser;
    const char *extension = src.extension();
    const unsigned flags = extension && strcmp("c", extension) ? GRParser::CPlusPlus : GRParser::None;
    const int count = parser.parse(src, flags, mSymbols);
    mFiles[Location::insertFile(src)] = time(0);
    warning() << "Parsed" << src << count << "symbols";
}

void GRTags::load(const Path &db)
{
    FILE *f = fopen(db.constData(), "r");
    if (f) {

    }
}

void GRTags::save(const Path &db)
{

}

int main(int argc, char **argv)
{
    GRTags grtags;
    return grtags.exec(argc, argv) ? 0 : 1;
}
