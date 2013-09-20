/* This file is part of RTags.

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include "GRTags.h"
#include "Filter.h"
#include "GRParser.h"
#include <rct/Log.h>
#include <getopt.h>
#include <rct/Memory.h>
#include <rct/Rct.h>
#include <math.h>
#include <leveldb/cache.h>
#include <leveldb/write_batch.h>

int main(int argc, char **argv)
{
    GRTags grtags;
    return grtags.exec(argc, argv) ? 0 : 1;
}

GRTags::GRTags()
    : mDB(0), mMode(Detect), mFlags(0), mKeyFlags(0)
{
    mFilters.append("/.gr/");
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
        { "all-symbols", required_argument, 0, 'l' },
        { "show-context", no_argument, 0, 'C' },
        { "find-file", optional_argument, 0, 'P' },
        { "dir", required_argument, 0, 'd' },
        { "dump", no_argument, 0, 's' },
        { "create", no_argument, 0, 'c' },
        { "silent", no_argument, 0, 'i' },
        { "update", no_argument, 0, 'u' },
        { "match-icase", no_argument, 0, 'I' },
        { "find-file-prefer-exact", no_argument, 0, 'A' },
        { "absolute-path", no_argument, 0, 'K' },
        { 0, 0, 0, 0 }
    };

    int logLevel = 0;
    Path dir;
    int c;
    const String shortOptions = Rct::shortOptions(options);
    String pattern;
    while ((c = getopt_long(argc, argv, shortOptions.constData(), options, 0)) != -1) {
        switch (c) {
        case '?':
            return false;
        case 'h':
            printf("grtags [...options]\n"
                   "  --help|-h                    Display this help\n"
                   "  --version|-V                 Display version information\n"
                   "  --verbose|-v                 Increase verbosity\n"
                   "  --silent|-i                  Be silent\n"
                   "  --exclude|-e [filter]        Exclude this pattern (e.g. .git, *.cpp)\n"
                   "  --references|-R [arg]        Show references to arg\n"
                   "  --list-symbols|-S [arg]      List symbols matching arg\n"
                   "  --all-symbols|-l [arg]       List symbols and references matching arg\n"
                   "  --find-symbols|-F [arg]      Find symbols matching arg\n"
                   "  --context|-C                 Show context\n"
                   "  --dump|-s                    Dump db contents\n"
                   "  --create|-c                  Force creation of new DB\n"
                   "  --update|-u                  Update existing DB (noop with no DB)\n"
                   "  --find-file|-P [arg]         List files matching optional arg\n"
                   "  --match-icase|-I             Match paths case insensitively\n"
                   "  --find-file-prefer-exact|-A  Use to make --find-file prefer exact matches over partial\n"
                   "  --absolute-path|-K           Print files with absolute path.\n"
                   "  --dir|-d [arg]               Parse this directory (default .)\n");

            return true;
        case 'V':
            printf("GRTags version 0.2\n");
            return true;
        case 'C':
            mKeyFlags |= Location::ShowContext;
            break;
        case 'A':
            mFlags |= PreferExact;
            break;
        case 'I':
            mFlags |= MatchCaseInsensitive;
            break;
        case 'K':
            mFlags |= AbsolutePath;
            break;
        case 'P':
            mMode = Paths;
            if (optarg) {
                pattern = optarg;
            } else if (optind < argc && *argv[optind] != '-') {
                pattern = argv[optind++];
            }
            break;
        case 'R':
            mMode = FindReferences;
            pattern = optarg;
            break;
        case 'F':
            mMode = FindSymbols;
            pattern = optarg;
            break;
        case 'l':
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
        case 'u':
            mMode = Update;
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
    if (dir.isEmpty()) {
        Path p = Path::pwd();
        while (!p.isEmpty()) {
            const Path db = p + "/.gr";
            if (db.isDir()) {
                dir = p;
                break;
            }
            p = p.parentDir();
        }
        if (dir.isEmpty()) {
            if (mMode == Detect)
                mMode = Create;
            if (mMode == Create)
                dir = Path::pwd();
        }
    }
    if (dir.isEmpty()) {
        warning() << "Can't find database";
        return false;
    }

    initLogging(logLevel, Path(), 0);
    if (!load(dir + "/.gr"))
        return false;
    switch (mMode) {
    case Dump:
        dump();
        return true;
    case Update:
    case Create:
        dir.visit(&GRTags::visit, this);
        if (parseFiles())
            return save();
        return true;
    case Paths:
        paths(pattern);
        break;
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

void GRTags::findSymbols(const String &pattern)
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

void GRTags::listSymbols(const String &pattern)
{
    std::shared_ptr<leveldb::Iterator> it(mDB->NewIterator(leveldb::ReadOptions()));
    const char *match = pattern.isEmpty() ? 0 : pattern.constData();
    int matchSize;
    if (match) {
        matchSize = pattern.size();
        it->Seek(match);
    } else {
        matchSize = 0;
        it->Seek(leveldb::Slice("A", 1));
    }
    while (it->Valid()) {
        const leveldb::Slice key = it->key();
        assert(!key.empty());
        if (!match || !strncmp(match, key.data(), std::min<int>(matchSize, key.size()))) {
            error("%s", key.ToString().c_str());
        } else {
            break;
        }
        it->Next();
    }
}


Path::VisitResult GRTags::visit(const Path &path, void *userData)
{
    GRTags *grtags = reinterpret_cast<GRTags*>(userData);
    const Filter::Result result = Filter::filter(path, grtags->mFilters);
    switch (result) {
    case Filter::Filtered:
        debug() << "Filtered out" << path;
        return Path::Continue;
    case Filter::Directory:
        debug() << "Entering directory" << path;
        return Path::Recurse;
    case Filter::File:
        Location::insertFile(path);
        break;
    case Filter::Source: {
        const uint32_t fileId = Location::insertFile(path);
        const time_t parsed = grtags->mFiles.value(fileId, 0);
        if (!parsed) {
            debug() << path << "not parsed before";
            grtags->mPending.append(path);
        } else if (parsed < path.lastModified()) {
            debug() << path << "is stale" << parsed << path.lastModified();
            grtags->mPending.append(path);
        } else {
            grtags->mDirty.remove(fileId);
            debug() << path << "seems to be up to date. Parsed at" << String::formatTime(parsed, String::DateTime)
                    << "last modified at" << String::formatTime(path.lastModified(), String::DateTime);
        }
        break; }
    }
    return Path::Continue;
}

bool GRTags::load(const Path &db)
{
    warning() << "Opening" << db << mMode;
    leveldb::Options options;
    if (mMode == Create) {
        // ### protect against removing wrong dir?
        Rct::removeDirectory(db);
        options.create_if_missing = true;
    }
    const leveldb::Status status = leveldb::DB::Open(options, db.constData(), &mDB);
    if (!status.ok()) {
        if (mMode == Update) {
            mMode = Create;
            return load(db);
        }
        error("Couldn't open database %s: %s", db.constData(), status.ToString().c_str());
        return false;
    }
    mPath = db;
    if (mMode != Create) {
        std::shared_ptr<leveldb::Iterator> it(mDB->NewIterator(leveldb::ReadOptions()));
        it->SeekToFirst();
        Map<Path, uint32_t> paths;
        leveldb::WriteBatch batch;
        bool hasWrites = false;
        while (it->Valid()) {
            const leveldb::Slice key = it->key();
            assert(!key.empty());
            if (key[0] == '/') {
                const leveldb::Slice value = it->value();
                assert(value.size() == sizeof(uint32_t));
                const uint32_t fileId = *reinterpret_cast<const uint32_t*>(value.data());
                const Path path(key.data(), key.size());
                if (path.exists() || mMode != Update) {
                    paths[path] = fileId;
                } else {
                    mDirty.insert(fileId);
                    hasWrites = true;
                    batch.Delete(key);
                }
            } else if (key[0] == ' ') {
                if (mMode == Dump || mMode == Update) {
                    assert(key.size() == sizeof(uint32_t) + 1);
                    const uint32_t fileId = *reinterpret_cast<const uint32_t*>(key.data() + 1);
                    const leveldb::Slice value = it->value();
                    const time_t lastParsed = *reinterpret_cast<const time_t*>(value.data());
                    // all parsed files are considered dirty until proven otherwise
                    if (mMode == Update && !mDirty.insert(fileId)) {
                        // this file was already marked as dirty so we know it's gone
                        assert(hasWrites);
                        batch.Delete(key);
                    } else {
                        mFiles[fileId] = lastParsed;
                    }
                }
            } else {
                break;
            }
            it->Next();
        }
        Location::init(paths);
        if (hasWrites)
            mDB->Write(leveldb::WriteOptions(), &batch);
    }
    return true;
}

bool GRTags::save()
{
    assert(mDB);
    leveldb::WriteBatch batch;
    bool hasWrites = false;
    {
        Map<Path, uint32_t> paths = Location::pathsToIds();
        for (Map<Path, uint32_t>::const_iterator it = paths.begin(); it != paths.end(); ++it) {
            const leveldb::Slice key(it->first.constData(), it->first.size());
            const leveldb::Slice value(reinterpret_cast<const char *>(&it->second), sizeof(it->second));
            batch.Put(key, value);
            hasWrites = true;
        }
    }
    {
        char keyBuf[5];
        keyBuf[0] = ' ';
        const leveldb::Slice key(keyBuf, 5);
        for (Map<uint32_t, time_t>::const_iterator it = mFiles.begin(); it != mFiles.end(); ++it) {
            *reinterpret_cast<uint32_t*>(keyBuf + 1) = it->first;
            const leveldb::Slice value(reinterpret_cast<const char *>(&it->second), sizeof(it->second));
            batch.Put(key, value);
            hasWrites = true;
        }
    }
    if (mMode == Update) {
        std::shared_ptr<leveldb::Iterator> it(mDB->NewIterator(leveldb::ReadOptions()));
        it->Seek(leveldb::Slice("A", 1));
        while (it->Valid()) {
            const leveldb::Slice key = it->key();
            const leveldb::Slice value = it->value();
            const String k(key.data(), key.size());
            Map<Location, bool> newValue = mSymbols.take(k);
            Map<Location, bool> oldValue;
            {
                Deserializer deserializer(value.data(), value.size());
                deserializer >> oldValue;
                assert(!oldValue.isEmpty());
            }

            bool modified = false;
            Map<Location, bool>::iterator i = oldValue.begin();
            while (i != oldValue.end()) {
                if (mDirty.contains(i->first.fileId())) {
                    const Map<Location, bool>::const_iterator found = newValue.find(i->first);
                    if (found == newValue.end() || found->second != i->second) {
                        oldValue.erase(i++);
                        modified = true;
                        continue;
                    }
                }
                ++i;
            }
            if (!newValue.isEmpty()) {
                for (Map<Location, bool>::const_iterator i = newValue.begin(); i != newValue.end(); ++i) {
                    if (oldValue.insert(std::make_pair(i->first, i->second)).second) {
                        modified = true;
                    }
                }
            }
            if (modified) {
                hasWrites = true;
                if (oldValue.isEmpty()) {
                    batch.Delete(key);
                } else {
                    String out;
                    out.reserve(1024);
                    Serializer serializer(out);
                    serializer << oldValue;
                    batch.Put(key, leveldb::Slice(out.constData(), out.size()));
                }
            }
            it->Next();
        }
    }
    for (Map<String, Map<Location, bool> >::const_iterator it = mSymbols.begin(); it != mSymbols.end(); ++it) {
        String out;
        Serializer serializer(out);
        serializer << it->second;
        batch.Put(leveldb::Slice(it->first.constData(), it->first.size()),
                  leveldb::Slice(out.constData(), out.size()));
        hasWrites = true;
    }

    if (hasWrites)
        mDB->Write(leveldb::WriteOptions(), &batch);

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
            error() << "  " << path << String::formatTime(it->second, String::DateTime);
        } else {
            error() << "  " << path;
        }
    }
    error() << delimiter;
    error() << "Symbols:";
    error() << delimiter;

    std::shared_ptr<leveldb::Iterator> it(mDB->NewIterator(leveldb::ReadOptions()));
    it->Seek(leveldb::Slice("A", 1));
    while (it->Valid()) {
        const leveldb::Slice key = it->key();
        error() << "  " << String(key.data(), key.size());
        const leveldb::Slice value = it->value();
        Map<Location, bool> locations;
        {
            Deserializer deserializer(value.data(), value.size());
            deserializer >> locations;
            assert(!locations.isEmpty());
        }

        for (Map<Location, bool>::const_iterator it2 = locations.begin(); it2 != locations.end(); ++it2) {
            if (it2->second) {
                error() << "    " << it2->first.key(Location::ShowContext) << "reference";
            } else {
                error() << "    " << it2->first.key(Location::ShowContext);
            }
        }
        it->Next();
    }
}

int GRTags::parseFiles()
{
    const int count = mPending.size();
    float percentageLast = -1;
    const bool debugging = testLog(Warning);
    for (int i=0; i<count; ++i) {
        GRParser parser;
        const Path &src = mPending[i];
        const char *extension = src.extension();
        const unsigned flags = extension && strcmp("c", extension) ? GRParser::CPlusPlus : GRParser::None;
        const int symbols = parser.parse(src, flags, mSymbols);
        mFiles[Location::insertFile(src)] = time(0);
        warning() << "Parsed" << src << symbols << "symbols";
        const float percentage = (static_cast<float>(i + 1) / static_cast<float>(count)) * 100.0;
        const float floored = floor(percentage);
        if (floored != percentageLast || i + 1 == count) {
            percentageLast = floored;
            error("%.1f%% parsed (%d/%d)", percentage, i + 1, count);
        }
        if (debugging)
            warning() << "Parsed" << src << symbols << "symbols";
    }
    return count;
}

void GRTags::paths(const String &pattern)
{
    const Path srcRoot = mPath.parentDir();
    const bool absolute = mFlags & AbsolutePath;

    const bool all = pattern.isEmpty();
    const String::CaseSensitivity cs = (mFlags & MatchCaseInsensitive
                                           ? String::CaseInsensitive
                                           : String::CaseSensitive);

    bool foundExact = false;
    const int patternSize = pattern.size();
    const Map<Path, uint32_t> paths = Location::pathsToIds();
    List<const char*> matches;
    const bool preferExact = mFlags & PreferExact;
    const int pathStart = absolute ? 0 : srcRoot.size();
    for (Map<Path, uint32_t>::const_iterator it = paths.begin(); it != paths.end(); ++it) {
        const Path &out = it->first;
        bool ok = true;
        if (!all) {
            if (!preferExact) {
                ok = out.indexOf(pattern, pathStart, cs);
            } else {
                const bool exact = (out.endsWith(pattern) && out.at(out.size() - (patternSize + 1)) == '/');
                if (exact) {
                    ok = true;
                    if (!foundExact) {
                        matches.clear();
                        foundExact = true;
                    }
                } else {
                    ok = !foundExact && out.indexOf(pattern, pathStart, cs);
                }
            }
        }
        if (ok) {
            const char *str = out.constData() + pathStart;
            if (preferExact && !foundExact) {
                matches.append(str);
            } else {
                error("%s", str);
            }
        }
    }
    for (List<const char *>::const_iterator it = matches.begin(); it != matches.end(); ++it) {
        error("%s", *it);
    }
}
