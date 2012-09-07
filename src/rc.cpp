#include "Client.h"
#include "QueryMessage.h"
#include "CreateOutputMessage.h"
#include "MakefileMessage.h"
#include "GRTagsMessage.h"
#include "EventLoop.h"
#include "RTags.h"
#include <ByteArray.h>
#include <List.h>
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <Log.h>
#include <dirent.h>
#include <fnmatch.h>

static void help(FILE *f, const char* app)
{
    fprintf(f, "%s options...\n"
            "  --help|-h                                 Display this help\n"
            "  --verbose|-v                              Be more verbose\n"
            "  --skip-paren|-p                           Skip parens in Makefile parsing\n"
            "  --elisp-list|-Y                           Output elisp: (list \"one\" \"two\" ...)\n"
            "  --follow-location|-f [arg]                Follow this location\n"
            "  --makefile|-m [arg]                       Process this makefile\n"
            "  --grtag|-t [arg]                          Index this directory\n"
            "  --disable-grtags|-b                       Disable grtags for query\n"
            "  --remake|-M [optional regexp]             Remake makefiles matching regexp or all if no regexp\n"
            "  --reference-name|-R [arg]                 Find references matching arg\n"
            "  --reference-location|-r [arg]             Find references matching this location\n"
            "  --all-references|-E                       Include definitions/declarations/constructors/destructors for references. Used for rename symbol.\n"
            "  --reverse-sort|-O                         Sort output reversed\n"
            "  --list-symbols|-S [arg]                   List symbol names matching arg\n"
            "  --find-symbols|-F [arg]                   Find symbols matching arg\n"
            "  --cursor-info|-U [arg]                    Get cursor info for this location\n"
            "  --log-file|-L [file]                      Log to this file\n"
            "  --append|-A                               Append to log file\n"
            "  --no-context|-N                           Don't print context for locations\n"
            "  --line-numbers|-l                         Output line numbers instead of offsets\n"
            "  --path-filter|-i [arg]                    Filter out results not matching with arg\n"
            "  --filter-system-headers|-H                Don't exempt system headers from path filters\n"
            "  --includepath|-I [arg]                    Add additional include path, must be combined with --makefile\n"
            "  --define|-D [arg]                         Add additional define, must be combined with --makefile\n"
            "  --compiler-flag|-o [arg]                  Add additional compiler flags, must be combined with --makefile\n"
            "  --test|-T [arg]                           Test whether rtags knows about this source file\n"
            "  --fixits|-x [file]                        Get fixits for file\n"
            "  --errors|-Q [file]                        Get errors for file\n"
            "  --rdm-log|-g                              Receive logs from rdm\n"
            "  --status|-s [arg]                         Dump status of rdm. If arg is passed it should match one of:\n"
            "                                            'general', 'fileids', dependencies', 'symbols', 'symbolnames', \n"
            "                                            'fileinfos', 'visitedfiles', 'grfiles' or 'gr' \n"
            "  --autostart-rdm|-a [args]                 Start rdm with [args] if rc fails to connect\n"
            "  --restart-rdm|-e [args]                   Restart rdm with [args] before doing the rest of the commands\n"
            "  --run-test|-k [file]                      Run tests from file\n"
            "  --diagnostics|-G                          Open a connection that prints diagnostics\n"
            "  --project|-w [optional regexp]            With arg, select project matching that if unique, otherwise list all projects\n"
            "  --delete-project|-W [regexp]              Delete all projects matching regexp\n"
            "  --clear-db|-C                             Clear projects\n"
            "  --reindex|-V [optional regexp]            Reindex all files or all files matching pattern\n"
            "  --wait-for-indexing|-X                    Wait for indexing to finish before doing query\n"
            "  --path|-P [optional pattern]              Print files matching pattern\n"
            "  --absolute-path|-K                        Print files with absolute path\n"
            "  --path-match-regexp|-Z                    Treat arguments to -P or -i as a regexps\n"
            "  --parse|-y [file]                         Parse file\n"
            "  --quit-rdm|-q                             Tell server to shut down\n",
            app);
}

static inline ByteArray encodeLocation(const ByteArray &key)
{
    const int lastComma = key.lastIndexOf(',');
    if (lastComma <= 0 || lastComma + 1 >= key.size())
        return ByteArray();

    char *endPtr;
    uint32_t offset = strtoull(key.constData() + lastComma + 1, &endPtr, 10);
    if (*endPtr != '\0')
        return ByteArray();
    Path path = Path::resolved(key.left(lastComma));
    ByteArray out;
    {
        out = path;
        char buf[4];
        memcpy(buf, &offset, sizeof(buf));
        out += ByteArray(buf, 4);
    }

    return out;
}

struct Command
{
    virtual ~Command() {}
    virtual void exec(Client *client) = 0;
    virtual ByteArray description() const = 0;
};

struct QueryCommand : public Command
{
    QueryCommand(QueryMessage::Type t, const ByteArray &q, const unsigned &qf, const Set<ByteArray> &p,
                 const Map<Path, ByteArray> &unsaved)
        : type(t), query(q), queryFlags(qf), pathFilters(p), unsavedFiles(unsaved)
    {}

    const QueryMessage::Type type;
    const ByteArray query;
    const unsigned &queryFlags; // eeh
    const Set<ByteArray> &pathFilters; // eeh
    const Map<Path, ByteArray> &unsavedFiles; // eeh

    virtual void exec(Client *client)
    {
        QueryMessage msg(type, query, queryFlags);
        msg.setPathFilters(pathFilters.toList());
        client->message(&msg);
    }

    virtual ByteArray description() const
    {
        return ("QueryMessage " + ByteArray::number(type) + " " + query);
    }
};

struct RdmLogCommand : public Command
{
    RdmLogCommand(const int &level)
        : mLevel(level)
    {
    }
    virtual void exec(Client *client)
    {
        CreateOutputMessage msg(mLevel);
        client->message(&msg);
    }
    virtual ByteArray description() const
    {
        return "RdmLogCommand";
    }
private:
    const int &mLevel;
};

struct MakefileCommand : public Command
{
    MakefileCommand(const Path &mf, const List<ByteArray> &args, const List<ByteArray> &ef)
        : makefile(mf), makefileArgs(args), extraFlags(ef)
    {}
    const Path makefile;
    const List<ByteArray> makefileArgs;
    const List<ByteArray> &extraFlags; // reference
    virtual void exec(Client *client)
    {
        if (!makefile.isFile()) {
            error() << makefile << "is not a file";
            return;
        }
        MakefileMessage msg(makefile, makefileArgs, extraFlags);
        client->message(&msg);
    }
    virtual ByteArray description() const
    {
        return ("MakefileCommand " + makefile + " " + ByteArray::join(makefileArgs, " ") + " " + ByteArray::join(extraFlags, " "));
    }
};

struct GRTagCommand : public Command
{
    GRTagCommand(const Path &dir)
        : directory(dir)
    {}
    const Path directory;
    virtual void exec(Client *client)
    {
        if (!directory.isDir()) {
            error() << directory << "is not a file";
            return;
        }
        GRTagsMessage msg(directory);
        client->message(&msg);
    }
    virtual ByteArray description() const
    {
        return ("GRTagMessage " + directory);
    }
};

int main(int argc, char** argv)
{
    RTags::findApplicationDirPath(*argv);

    struct option opts[] = {
        { "verbose", no_argument, 0, 'v' },
        { "skip-paren", no_argument, 0, 'p' },
        { "help", no_argument, 0, 'h' },
        { "autostart-rdm", optional_argument, 0, 'a' },
        { "follow-location", required_argument, 0, 'f' },
        { "makefile", optional_argument, 0, 'm' },
        { "remake", optional_argument, 0, 'M' },
        { "reference-name", required_argument, 0, 'R' },
        { "reference-location", required_argument, 0, 'r' },
        { "reverse-sort", no_argument, 0, 'O' },
        { "list-symbols", optional_argument, 0, 'S' },
        { "find-symbols", required_argument, 0, 'F' },
        { "cursor-info", required_argument, 0, 'U' },
        { "unsaved-file", required_argument, 0, 'u' },
        { "log-file", required_argument, 0, 'L' },
        { "append", no_argument, 0, 'A' },
        { "no-context", no_argument, 0, 'N' },
        { "status", optional_argument, 0, 's' },
        { "rdm-log", no_argument, 0, 'g' },
        { "line-numbers", no_argument, 0, 'l' },
        { "path-filter", required_argument, 0, 'i' },
        { "filter-system-headers", no_argument, 0, 'H' },
        { "includepath", required_argument, 0, 'I' },
        { "define", required_argument, 0, 'D' },
        { "compiler-flag", required_argument, 0, 'o' },
        { "test", required_argument, 0, 'T' },
        { "quit-rdm", no_argument, 0, 'q' },
        { "restart-rdm", optional_argument, 0, 'e' },
        { "all-references", no_argument, 0, 'E' },
        { "elisp-list", no_argument, 0, 'Y' },
        { "run-test", required_argument, 0, 'k' },
        { "clear-db", no_argument, 0, 'C' },
        { "fixits", required_argument, 0, 'x' },
        { "errors", required_argument, 0, 'Q' },
        { "reindex", optional_argument, 0, 'V' },
        { "diagnostics", no_argument, 0, 'G' },
        { "project", optional_argument, 0, 'w' },
        { "delete-project", required_argument, 0, 'W' },
        { "wait-for-indexing", no_argument, 0, 'X' },
        { "path", optional_argument, 0, 'P' },
        { "path-match-regexp", no_argument, 0, 'Z' },
        { "absolute-path", no_argument, 0, 'K' },
        { "parse", required_argument, 0, 'y' },
        { "enable-grtags", no_argument, 0, 'b' },
        { "grtag", optional_argument, 0, 't' },
        { "socket-file", required_argument, 0, 'B' },
        { 0, 0, 0, 0 }
    };

    // Unused: djJcn

    int logLevel = 0;
    ByteArray logFile;
    unsigned logFlags = 0;

    List<Command*> commands;
    List<ByteArray> extraFlags;
    Set<ByteArray> pathFilters;
    unsigned queryFlags = 0;
    unsigned clientFlags = 0;
    List<ByteArray> rdmArgs;
    ByteArray socketFile = Path::home() + ".rdm";
    Map<Path, ByteArray> unsavedFiles;

    const ByteArray shortOptions = RTags::shortOptions(opts);

    while (true) {
        const int c = getopt_long(argc, argv, shortOptions.constData(), opts, 0);
        if (c == -1)
            break;
        switch (c) {
        case 0:
            break;
        case 'h':
            help(stdout, argv[0]);
            return 0;
        case 'B':
            socketFile = optarg;
            break;
        case 'b':
            queryFlags |= QueryMessage::DisableGRTags;
            break;
        case 'a':
            clientFlags |= Client::AutostartRdm;
            if (optarg)
                rdmArgs = ByteArray(optarg, strlen(optarg)).split(' ');
            break;
        case 'e':
            clientFlags |= Client::RestartRdm;
            if (optarg)
                rdmArgs = ByteArray(optarg, strlen(optarg)).split(' ');
            break;
        case 'E':
            queryFlags |= QueryMessage::ReferencesForRenameSymbol;
            break;
        case 'Z':
            queryFlags |= QueryMessage::PathMatchRegExp;
            break;
        case 'K':
            queryFlags |= QueryMessage::AbsolutePath;
            break;
        case 'X':
            queryFlags |= QueryMessage::WaitForIndexing;
            break;
        case 'O':
            queryFlags |= QueryMessage::ReverseSort;
            break;
        case 'Y':
            queryFlags |= QueryMessage::ElispList;
            break;
        case 'H':
            queryFlags |= QueryMessage::FilterSystemIncludes;
            break;
        case 'I': {
            ByteArray flag("-I");
            flag += optarg;
            extraFlags.append(flag);
            break; }
        case 'D': {
            ByteArray flag("-D");
            flag += optarg;
            extraFlags.append(flag);
            break; }
        case 'o':
            extraFlags.append(optarg);
            break;
        case 'N':
            queryFlags |= QueryMessage::NoContext;
            break;
        case 'i':
            pathFilters.insert(optarg);
            break;
        case 'l':
            queryFlags |= QueryMessage::LineNumbers;
            break;
        case 'v':
            ++logLevel;
            break;
        case 'A':
            logFlags |= Append;
            break;
        case 'L':
            logFile = optarg;
            break;
        case 'p':
            queryFlags |= QueryMessage::SkipParentheses;
            break;
        case 'u': {
            const ByteArray arg(optarg);
            const int colon = arg.lastIndexOf(':');
            if (colon == -1) {
                fprintf(stderr, "Can't parse -u [%s]\n", optarg);
                return 1;
            }
            const int bytes = atoi(arg.constData() + colon + 1);
            if (!bytes) {
                fprintf(stderr, "Can't parse -u [%s]\n", optarg);
                return 1;
            }
            const Path path = Path::resolved(arg.left(colon));
            if (!path.isFile()) {
                fprintf(stderr, "Can't open [%s] for reading\n", arg.left(colon).nullTerminated());
                return 1;
            }

            ByteArray contents(bytes, '\0');
            const int r = fread(contents.data(), 1, bytes, stdin);
            if (r != bytes) {
                fprintf(stderr, "Read error %d (%s). Got %d, expected %d\n",
                        errno, strerror(errno), r, bytes);
            }
            unsavedFiles[path] = contents;
            break; }
        case 'f':
        case 'U':
        case 'c':
        case 'r': {
            const ByteArray encoded = encodeLocation(optarg);
            if (encoded.isEmpty()) {
                fprintf(stderr, "Can't resolve argument %s\n", optarg);
                return 1;
            }
            QueryMessage::Type type = QueryMessage::Invalid;
            switch (c) {
            case 'f': type = QueryMessage::FollowLocation; break;
            case 'U': type = QueryMessage::CursorInfo; break;
            case 'r': type = QueryMessage::ReferencesLocation; break;
            }
            commands.append(new QueryCommand(type, encoded, queryFlags, pathFilters, unsavedFiles));
            break; }
        case 'C':
            commands.append(new QueryCommand(QueryMessage::ClearProjects, ByteArray(), queryFlags, pathFilters, unsavedFiles));
            break;
        case 'g':
            commands.append(new RdmLogCommand(logLevel));
            break;
        case 'G':
            commands.append(new RdmLogCommand(CompilationError));
            break;
        case 'q':
            commands.append(new QueryCommand(QueryMessage::Shutdown, ByteArray(), queryFlags, pathFilters, unsavedFiles));
            break;
        case 'W':
            commands.append(new QueryCommand(QueryMessage::DeleteProject, optarg, queryFlags, pathFilters, unsavedFiles));
            break;
        case 'V':
        case 'w':
        case 'P':
        case 'M': {
            QueryMessage::Type type = QueryMessage::Invalid;
            switch (c) {
            case 'V': type = QueryMessage::Reindex; break;
            case 'w': type = QueryMessage::Project; break;
            case 'M': type = QueryMessage::Remake; break;
            case 'P': type = QueryMessage::FindFile; break;
            }

            if (optarg) {
                commands.append(new QueryCommand(type, optarg, queryFlags, pathFilters, unsavedFiles));
            } else if (optind < argc && argv[optind][0] != '-') {
                commands.append(new QueryCommand(type, argv[optind++], queryFlags, pathFilters, unsavedFiles));
            } else {
                commands.append(new QueryCommand(type, ByteArray(), queryFlags, pathFilters, unsavedFiles));
            }
            break; }
        case 't':
            if (optarg) {
                commands.append(new GRTagCommand(Path::resolved(optarg)));
            } else if (optind < argc && argv[optind][0] != '-') {
                commands.append(new GRTagCommand(Path::resolved(argv[optind++])));
            } else {
                commands.append(new GRTagCommand(Path::resolved(".")));
            }
            break;
        case 'T':
        case 'x':
        case 'Q':
        case 'y':
        case 'k': {
            const Path p = Path::resolved(optarg);
            if (!p.isFile()) {
                fprintf(stderr, "%s is not a file\n", optarg);
                return 1;
            }
            QueryMessage::Type type = QueryMessage::Invalid;
            switch (c) {
            case 'T': type = QueryMessage::Test; break;
            case 'x': type = QueryMessage::FixIts; break;
            case 'Q': type = QueryMessage::Errors; break;
            case 'k': type = QueryMessage::RunTest; break;
            case 'y': type = QueryMessage::Parse; break;
            }

            commands.append(new QueryCommand(type, p, queryFlags, pathFilters, unsavedFiles));
            break; }
        case 'm': {
            Path makefile;
            if (optarg) {
                makefile = Path::resolved(optarg);
                if (!makefile.isFile()) {
                    fprintf(stderr, "%s is not a file\n", optarg);
                    return 1;
                }
            } else {
                if (optind < argc) {
                    makefile = Path::resolved(argv[optind]);
                    if (!makefile.isFile()) {
                        makefile = Path::resolved("Makefile");
                        if (!makefile.isFile()) {
                            fprintf(stderr, "%s is not a file", argv[optind]);
                            return 1;
                        }
                    } else {
                        ++optind;
                    }
                } else {
                    makefile = Path::resolved("Makefile");
                    if (!makefile.isFile()) {
                        fprintf(stderr, "Can't find a Makefile here\n");
                        return 1;
                    }
                }
            }

            List<ByteArray> makefileArgs;
            while (optind < argc && argv[optind][0] != '-')
                makefileArgs.append(argv[optind++]);
            commands.append(new MakefileCommand(makefile, makefileArgs, extraFlags));
            break; }
        case 's':
            if (optarg) {
                commands.append(new QueryCommand(QueryMessage::Status, optarg, queryFlags,
                                                 pathFilters, unsavedFiles));
            } else if (optind < argc && argv[optind][0] != '-') {
                commands.append(new QueryCommand(QueryMessage::Status, argv[optind++], queryFlags,
                                                 pathFilters, unsavedFiles));
            } else {
                commands.append(new QueryCommand(QueryMessage::Status, ByteArray(), queryFlags,
                                                 pathFilters, unsavedFiles));
            }
            break;
        case 'R':
            commands.append(new QueryCommand(QueryMessage::ReferencesName, optarg, queryFlags, pathFilters, unsavedFiles));
            break;
        case 'S':
            if (optarg) {
                commands.append(new QueryCommand(QueryMessage::ListSymbols, optarg, queryFlags, pathFilters, unsavedFiles));
            } else if (optind < argc && argv[optind][0] != '-') {
                commands.append(new QueryCommand(QueryMessage::ListSymbols, argv[optind++], queryFlags, pathFilters, unsavedFiles));
            } else {
                commands.append(new QueryCommand(QueryMessage::ListSymbols, ByteArray(), queryFlags, pathFilters, unsavedFiles));
            }
            break;
        case 'F':
            commands.append(new QueryCommand(QueryMessage::FindSymbols, optarg, queryFlags, pathFilters, unsavedFiles));
            break;
        case '?':
            // getopt printed an error message already
            break;
        default:
            break;
        }
    }
    if (optind < argc) {
        fprintf(stderr, "rc: unexpected option -- '%s'\n", argv[optind]);
        return 1;
    }

    if (!initLogging(logLevel, logFile, logFlags)) {
        fprintf(stderr, "Can't initialize logging with %d %s 0x%0x\n",
                logLevel, logFile.constData(), logFlags);
        cleanupLogging();
        return 1;
    }

    if (commands.isEmpty() && !(clientFlags & (Client::RestartRdm|Client::AutostartRdm))) {
        help(stderr, argv[0]);
        cleanupLogging();
        return 1;
    }

    if (!logFile.isEmpty() || logLevel > 0) {
        Log l(1);
        l << argc;
        for (int i = 0; i < argc; ++i)
            l << " " << argv[i];
    }

    EventLoop loop;

    Client client(socketFile, clientFlags, rdmArgs);
    const int commandCount = commands.size();
    for (int i=0; i<commandCount; ++i) {
        Command *cmd = commands.at(i);
        debug() << "running command " << cmd->description();
        cmd->exec(&client);
        delete cmd;
    }
    cleanupLogging();
    return 0;
}
