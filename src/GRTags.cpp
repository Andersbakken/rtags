#include "GRTags.h"
#include "Filter.h"
#include "GRParser.h"
#include "Log.h"
#include <getopt.h>
#include <math.h>
#include <leveldb/cache.h>

GRTags::GRTags()
    : mDB(0), mMode(Detect), mKeyFlags(0)
{
}

bool GRTags::exec(int argc, char **argv)
{
    option options[] = {
        { "help", no_argument, 0, 'h' },
        { "version", no_argument, 0, 'V' },
        { "verbose", no_argument, 0, 'v' },
        { "exclude", required_argument, 0, 'e' },
        { "references", required_argument, 0, 'R' },
        { "list-symbols", optional_argument, 0, 'S' },
        { "find-symbols", required_argument, 0, 'F' },
        { "all-symbols", required_argument, 0, 'A' },
        { "show-context", no_argument, 0, 'C' },
        { "dir", required_argument, 0, 'd' },
        { "dump", no_argument, 0, 's' },
        { "create", no_argument, 0, 'c' },
        { "silent", no_argument, 0, 'i' },
        { 0, 0, 0, 0 }
    };

    int logLevel = 0;
    Path dir = ".";
    int c;
    ByteArray pattern;
    while ((c = getopt_long(argc, argv, "hVCvd:e:csR:F:S::A:i", options, 0)) != -1) {
        switch (c) {
        case '?':
            return false;
        case 'h':
            printf("grtags [...options]\n"
                   "  --help|-h               Display this help\n"
                   "  --version|-V            Display version information\n"
                   "  --verbose|-v            Increase verbosity\n"
                   "  --silent|-i             Be silent\n"
                   "  --exclude|-e [filter]   Exclude this pattern (e.g. .git, *.cpp)\n"
                   "  --references|-R [arg]   Show references to arg\n"
                   "  --list-symbols|-S [arg] List symbols matching arg\n"
                   "  --all-symbols|-A [arg]  List symbols and references matching arg\n"
                   "  --find-symbols|-F [arg] Find symbols matching arg\n"
                   "  --context|-C            Show context\n"
                   "  --dump|-s               Dump db contents\n"
                   "  --create|-c             Force creation of new DB\n"
                   "  --dir|-d [directory]    Parse this directory (default .)\n");
            return true;
        case 'V':
            printf("GRTags version 0.1\n");
            return true;
        case 'C':
            mKeyFlags |= Location::ShowContext;
            break;
        case 'R':
            mMode = FindReferences;
            pattern = optarg;
            break;
        case 'F':
            mMode = FindSymbols;
            pattern = optarg;
            break;
        case 'A':
            mMode = FindAll;
            pattern = optarg;
            break;
        case 'S':
            mMode = ListSymbols;
            if (optarg) {
                pattern = optarg;
            } else if (optind < argc && *argv[optind] != '-') {
                pattern = argv[optind++];
            }
            break;
        case 'c':
            mMode = Create;
            break;
        case 'e':
            mFilters.append(optarg);
            break;
        case 'v':
            if (logLevel >= 0)
                ++logLevel;
            break;
        case 'i':
            logLevel = -1;
            break;
        case 's':
            mMode = Dump;
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
    if (mMode == Detect || mMode == Dump) {
        Path p = dir;
        while (!p.isEmpty()) {
            db = p + "/.grtags.db";
            if (db.isDir())
                break;
            db.clear();
            p = p.parentDir();
        }
    }
    if (mMode == Detect)
        mMode = db.isDir() ? Update : Create;

    initLogging(logLevel, Path(), 0);
    if (db.isEmpty())
        db = dir + "/.grtags.db";
    if (!load(db))
        return false;

    switch (mMode) {
    case Dump:
        dump();
        return true;
    case Update:
    case Create:
        dir.visit(&GRTags::visit, this);
        return save();
    case FindReferences:
    case FindAll:
    case FindSymbols:
        findSymbols(pattern);
        break;
    case ListSymbols:
        listSymbols(pattern);
        break;
    case Detect:
        break;
    }
    return false;
}

void GRTags::findSymbols(const ByteArray &pattern)
{
    std::string value;
    if (mDB->Get(leveldb::ReadOptions(), leveldb::Slice(pattern.constData(), pattern.size()), &value).ok()) {
        Deserializer deserializer(value.c_str(), value.size());
        Map<Location, bool> symbols;
        deserializer >> symbols;
        for (Map<Location, bool>::const_iterator it = symbols.begin(); it != symbols.end(); ++it) {
            bool ok;
            switch (mMode) {
            case FindSymbols:
                ok = !it->second;
                break;
            case FindReferences:
                ok = it->second;
                break;
            case FindAll:
                ok = true;
                break;
            default:
                assert(0);
                return;
            }
            if (ok) {
                error() << it->first.key(mKeyFlags);
            }
        }
    }
}

void GRTags::listSymbols(const ByteArray &pattern)
{
    leveldb::Iterator *it = mDB->NewIterator(leveldb::ReadOptions());
    const char *match = pattern.isEmpty() ? 0 : pattern.constData();
    int matchSize;
    if (match) {
        matchSize = pattern.size();
        it->Seek(match);
    } else {
        matchSize = 0;
        it->SeekToFirst();
    }
    while (it->Valid()) {
        const leveldb::Slice key = it->key();
        assert(!key.empty());
        if (key[0] != '/' && !isdigit(key[0])) {
            if (!match || !strncmp(match, key.data(), std::min<int>(matchSize, key.size()))) {
                printf("%s\n", key.ToString().c_str());
            } else {
                break;
            }
        }
        it->Next();
    }
    delete it;
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
    time_t &parsed = mFiles[Location::insertFile(src)];
    if (parsed < src.lastModified()) {
        Timer timer;
        GRParser parser;
        const char *extension = src.extension();
        const unsigned flags = extension && strcmp("c", extension) ? GRParser::CPlusPlus : GRParser::None;
        const int count = parser.parse(src, flags, mSymbols);
        mFiles[Location::insertFile(src)] = time(0);
        warning() << "Parsed" << src << count << "symbols";
    } else {
        warning() << src << "seems to be up to date. Parsed at" << RTags::timeToString(parsed, RTags::DateTime)
                  << "last modified at" << RTags::timeToString(src.lastModified(), RTags::DateTime);
    }
}

bool GRTags::load(const Path &db)
{
    warning() << "Opening" << db << mMode;
    if (mMode == Create) {
        // ### protect against removing wrong dir?
        RTags::removeDirectory(db);
    }
    leveldb::Options options;
    options.create_if_missing = true;
    // options.block_cache = leveldb::NewLRUCache(16 * 1024 * 1024);
    leveldb::Status status = leveldb::DB::Open(options, db.constData(), &mDB);
    if (!status.ok()) {
        error("Couldn't open database %s: %s", db.constData(), status.ToString().c_str());
        return false;
    }
    mPath = db;
    switch (mMode) {
    case Update:
    case Create:
    case Dump: {
        leveldb::Iterator *it = mDB->NewIterator(leveldb::ReadOptions());
        it->SeekToFirst();
        Map<Path, uint32_t> paths;
        while (it->Valid()) {
            const leveldb::Slice key = it->key();
            const leveldb::Slice value = it->value();
            assert(!key.empty());
            if (key[0] == '/') {
                paths[Path(key.data(), key.size())] = atoi(value.ToString().c_str());
            } else if (isdigit(key[0])) {
                mFiles[atoi(key.data())] = atoll(value.data());
            } else {
                Map<Location, bool> &syms = mSymbols[ByteArray(key.data(), key.size())];
                Deserializer deserializer(value.data(), value.size());
                deserializer >> syms;
            }
            it->Next();
        }
        Location::init(paths);
        delete it;
        break; }
    case ListSymbols:
        break;
    case FindSymbols:
    case FindAll:
    case FindReferences: {
        leveldb::Iterator *it = mDB->NewIterator(leveldb::ReadOptions());
        it->Seek(leveldb::Slice("/", 1));
        Map<Path, uint32_t> paths;
        while (it->Valid()) {
            const leveldb::Slice key = it->key();
            assert(!key.empty());
            if (key[0] == '/') {
                const leveldb::Slice value = it->value();
                paths[Path(key.data(), key.size())] = atoi(value.ToString().c_str());
            } else {
                break;
            }
            it->Next();
        }
        Location::init(paths);
        delete it;
        break; }
    case Detect:
        return false;
    }

    return true;
}

bool GRTags::save()
{
    assert(mDB);
    leveldb::WriteOptions writeOptions;
    {
        Map<Path, uint32_t> paths = Location::pathsToIds();
        char numberBuf[16];
        for (Map<Path, uint32_t>::const_iterator it = paths.begin(); it != paths.end(); ++it) {
            const leveldb::Slice key(it->first.constData(), it->first.size());
            const int w = snprintf(numberBuf, sizeof(numberBuf), "%d", it->second);
            mDB->Put(writeOptions, key, leveldb::Slice(numberBuf, w));
        }
    }
    {
        char keyBuf[16];
        char valueBuf[16];

        for (Map<uint32_t, time_t>::const_iterator it = mFiles.begin(); it != mFiles.end(); ++it) {
            const int k = snprintf(keyBuf, sizeof(keyBuf), "%d", it->first);
            const int v = snprintf(valueBuf, sizeof(valueBuf), "%ld", it->second);
            mDB->Put(writeOptions, leveldb::Slice(keyBuf, k), leveldb::Slice(valueBuf, v));
        }
    }
    for (Map<ByteArray, Map<Location, bool> >::const_iterator it = mSymbols.begin(); it != mSymbols.end(); ++it) {
        ByteArray out;
        Serializer serializer(out);
        serializer << it->second;
        mDB->Put(writeOptions,
                 leveldb::Slice(it->first.constData(), it->first.size()),
                 leveldb::Slice(out.constData(), out.size()));
    }
    delete mDB;
    mDB = 0;
    return true;
}

void GRTags::dump()
{
    const char *delimiter = "-----------------------------------------------";
    error() << "Locations:";

    error() << delimiter;
    Map<Path, uint32_t> pathsToIds = Location::pathsToIds();
    for (Map<Path, uint32_t>::const_iterator it = pathsToIds.begin(); it != pathsToIds.end(); ++it)
        error() << "  " << it->first << it->second;

    error() << delimiter;
    Map<uint32_t, Path> idsToPaths = Location::idsToPaths();
    for (Map<uint32_t, Path>::const_iterator it = idsToPaths.begin(); it != idsToPaths.end(); ++it)
        error() << "  " << it->first << it->second;

    error() << delimiter;
    error() << "Files:";
    error() << delimiter;

    for (Map<uint32_t, time_t>::const_iterator it = mFiles.begin(); it != mFiles.end(); ++it) {
        const Path path = Location::path(it->first);
        if (it->second) {
            error() << "  " << path << RTags::timeToString(it->second, RTags::DateTime);
        } else {
            error() << "  " << path;
        }
    }
    error() << delimiter;
    error() << "Symbols:";
    error() << delimiter;

    for (Map<ByteArray, Map<Location, bool> >::const_iterator it = mSymbols.begin(); it != mSymbols.end(); ++it) {
        error() << "  " << it->first;
        for (Map<Location, bool>::const_iterator it2 = it->second.begin(); it2 != it->second.end(); ++it2) {
            if (it2->second) {
                error() << "    " << it2->first.key(Location::ShowContext) << "reference";
            } else {
                error() << "    " << it2->first.key(Location::ShowContext);
            }

        }
    }
}

int main(int argc, char **argv)
{
    GRTags grtags;
    return grtags.exec(argc, argv) ? 0 : 1;
}
