#include "RClient.h"
#include "CreateOutputMessage.h"
#include "MakefileMessage.h"
#include "GRTagsMessage.h"
#include "EventLoop.h"

class RCCommand
{
public:
    virtual ~RCCommand() {}
    virtual void exec(RClient *rc, Client *client) = 0;
    virtual ByteArray description() const = 0;
};

class QueryCommand : public RCCommand
{
public:
    QueryCommand(QueryMessage::Type t, const ByteArray &q)
        : type(t), query(q)
    {}

    const QueryMessage::Type type;
    const ByteArray query;

    virtual void exec(RClient *rc, Client *client)
    {
        QueryMessage msg(type);
        msg.setQuery(query);
        msg.setFlags(rc->queryFlags());
        msg.setMax(rc->max());
        msg.setUnsavedFiles(rc->unsavedFiles());
        msg.setPathFilters(rc->pathFilters().toList());
        msg.setLockTimeout(rc->lockTimeout());
        client->message(&msg);
    }

    virtual ByteArray description() const
    {
        return ("QueryMessage " + ByteArray::number(type) + " " + query);
    }
};

class RdmLogCommand : public RCCommand
{
public:
    enum { Default = -2 };
    RdmLogCommand(int level)
        : mLevel(level)
    {
    }
    virtual void exec(RClient *rc, Client *client)
    {
        CreateOutputMessage msg(mLevel == Default ? rc->logLevel() : mLevel);
        client->message(&msg);
    }
    virtual ByteArray description() const
    {
        return "RdmLogCommand";
    }
    const int mLevel;
};

class MakefileCommand : public RCCommand
{
public:
    MakefileCommand(const Path &mf, const List<ByteArray> &args)
        : makefile(mf), makefileArgs(args)
    {}
    const Path makefile;
    List<ByteArray> makefileArgs;
    virtual void exec(RClient *rc, Client *client)
    {
        if (!makefile.isFile()) {
            error() << makefile << "is not a file";
            return;
        }
        if (rc->makefileFlags() & RClient::UseDashB)
            makefileArgs.append("-B");
        MakefileMessage msg(makefile, makefileArgs, rc->extraFlags());
        client->message(&msg);
    }
    virtual ByteArray description() const
    {
        return ("MakefileCommand " + makefile + " " + ByteArray::join(makefileArgs, " "));
    }
};

class GRTagCommand : public RCCommand
{
public:
    GRTagCommand(const Path &dir)
        : directory(dir)
    {}
    const Path directory;
    virtual void exec(RClient *rc, Client *client)
    {
        if (!directory.isDir()) {
            error() << directory << "is not a directory";
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

RClient::RClient()
    : mQueryFlags(0), mClientFlags(0), mMakefileFlags(0), mMax(-1), mLogLevel(0), mLockTimeout(0)
{
}

RClient::~RClient()
{
    cleanupLogging();
}

void RClient::addQuery(QueryMessage::Type t, const ByteArray &query)
{
    rCommands.append(new QueryCommand(t, query));
}
void RClient::addLog(int level)
{
    rCommands.append(new RdmLogCommand(level));
}

void RClient::addMakeFile(const Path &makefile, const List<ByteArray> &args)
{
    rCommands.append(new MakefileCommand(makefile, args));
}

void RClient::addGRTag(const Path &dir)
{
    rCommands.append(new GRTagCommand(dir));
}

void RClient::exec()
{
    EventLoop loop;

    Client client(mSocketFile, mClientFlags, mRdmArgs);

    const int commandCount = rCommands.size();
    for (int i=0; i<commandCount; ++i) {
        RCCommand *cmd = rCommands.at(i);
        debug() << "running command " << cmd->description();
        cmd->exec(this, &client);
        delete cmd;
    }
    rCommands.clear();
}

static void help(FILE *f, const char* app)
{
    fprintf(f, "%s options...\n"
            "  --help|-h                                 Display this help\n"
            "  --verbose|-v                              Be more verbose\n"
            "  --skip-paren|-p                           Skip parens in Makefile parsing\n"
            "  --elisp-list|-Y                           Output elisp: (list \"one\" \"two\" ...)\n"
            "  --follow-location|-f [arg]                Follow this location\n"
            "  --makefile|-m [arg]                       Process this makefile\n"
            "  --socket-file|-n [arg]                    Use this socket file (default ~/.rdm)\n"
            "  --grtag|-t [arg]                          Index this directory\n"
            "  --disable-grtags|-b                       Disable grtags for query\n"
            "  --max|-M [count]                          Max lines of output for queries\n"
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
            "  --diagnostics|-G                          Open a connection that prints diagnostics\n"
            "  --project|-w [optional regexp]            With arg, select project matching that if unique, otherwise list all projects\n"
            "  --delete-project|-W [regexp]              Delete all projects matching regexp\n"
            "  --clear-db|-C                             Clear projects\n"
            "  --reindex|-V [optional regexp]            Reindex all files or all files matching pattern\n"
            "  --wait-for-indexing|-X                    Wait for indexing to finish before doing query\n"
            "  --path|-P [optional pattern]              Print files matching pattern\n"
            "  --dump-file|-d [file]                     Dump source file\n"
            "  --absolute-path|-K                        Print files with absolute path\n"
            "  --match-regexp|-Z                         Treat various text patterns as regexps (-P, -i, -V)\n"
            "  --lock-timeout|-y [arg]                   Max time in ms to wait for a database lock (default no timeout)\n"
            "  --quit-rdm|-q                             Tell server to shut down\n",
            app);
}

bool RClient::parse(int &argc, char **argv)
{
    RTags::findApplicationDirPath(*argv);
    mSocketFile = Path::home() + ".rdm";
    struct option opts[] = {
        { "verbose", no_argument, 0, 'v' },
        { "skip-paren", no_argument, 0, 'p' },
        { "help", no_argument, 0, 'h' },
        { "autostart-rdm", optional_argument, 0, 'a' },
        { "follow-location", required_argument, 0, 'f' },
        { "makefile", optional_argument, 0, 'm' },
        { "max", required_argument, 0, 'M' },
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
        { "clear-db", no_argument, 0, 'C' },
        { "fixits", required_argument, 0, 'x' },
        { "errors", required_argument, 0, 'Q' },
        { "reindex", optional_argument, 0, 'V' },
        { "diagnostics", no_argument, 0, 'G' },
        { "project", optional_argument, 0, 'w' },
        { "delete-project", required_argument, 0, 'W' },
        { "wait-for-indexing", no_argument, 0, 'X' },
        { "path", optional_argument, 0, 'P' },
        { "match-regexp", no_argument, 0, 'Z' },
        { "absolute-path", no_argument, 0, 'K' },
        { "enable-grtags", no_argument, 0, 'b' },
        { "grtag", optional_argument, 0, 't' },
        { "socket-file", required_argument, 0, 'n' },
        { "always-make", no_argument, 0, 'B' },
        { "dump-file", required_argument, 0, 'd' },
        { "locktimeout", required_argument, 0, 'y' },
        { 0, 0, 0, 0 }
    };

    unsigned logFlags = 0;
    Path logFile;

    // Unused: jJck

    const ByteArray shortOptions = RTags::shortOptions(opts);

    int c;
    while ((c = getopt_long(argc, argv, shortOptions.constData(), opts, 0)) != -1) {
        switch (c) {
        case 0:
            break;
        case 'h':
            help(stdout, argv[0]);
            return 0;
        case 'n':
            mSocketFile = optarg;
            break;
        case 'b':
            mQueryFlags |= QueryMessage::DisableGRTags;
            break;
        case 'B':
            mMakefileFlags |= UseDashB;
            break;
        case 'a':
            mClientFlags |= Client::AutostartRdm;
            if (optarg)
                mRdmArgs = ByteArray(optarg, strlen(optarg)).split(' ');
            break;
        case 'e':
            mClientFlags |= Client::RestartRdm;
            if (optarg)
                mRdmArgs = ByteArray(optarg, strlen(optarg)).split(' ');
            break;
        case 'E':
            mQueryFlags |= QueryMessage::ReferencesForRenameSymbol;
            break;
        case 'Z':
            mQueryFlags |= QueryMessage::MatchRegexp;
            break;
        case 'K':
            mQueryFlags |= QueryMessage::AbsolutePath;
            break;
        case 'X':
            mQueryFlags |= QueryMessage::WaitForIndexing;
            break;
        case 'O':
            mQueryFlags |= QueryMessage::ReverseSort;
            break;
        case 'Y':
            mQueryFlags |= QueryMessage::ElispList;
            break;
        case 'H':
            mQueryFlags |= QueryMessage::FilterSystemIncludes;
            break;
        case 'I': {
            ByteArray flag("-I");
            flag += optarg;
            mExtraFlags.append(flag);
            break; }
        case 'D': {
            ByteArray flag("-D");
            flag += optarg;
            mExtraFlags.append(flag);
            break; }
        case 'o':
            mExtraFlags.append(optarg);
            break;
        case 'N':
            mQueryFlags |= QueryMessage::NoContext;
            break;
        case 'i':
            mPathFilters.insert(optarg);
            break;
        case 'l':
            mQueryFlags |= QueryMessage::LineNumbers;
            break;
        case 'v':
            ++mLogLevel;
            break;
        case 'A':
            logFlags |= Append;
            break;
        case 'L':
            logFile = optarg;
            break;
        case 'p':
            mQueryFlags |= QueryMessage::SkipParentheses;
            break;
        case 'M':
            mMax = atoi(optarg);
            if (mMax <= 0) {
                fprintf(stderr, "-M [arg] must be positive integer\n");
                return false;
            }
            break;
        case 'y':
            mLockTimeout = atoi(optarg);
            if (mMax <= 0) {
                fprintf(stderr, "-y [arg] must be positive integer\n");
                return false;
            }
            break;
        case 'u': {
            const ByteArray arg(optarg);
            const int colon = arg.lastIndexOf(':');
            if (colon == -1) {
                fprintf(stderr, "Can't parse -u [%s]\n", optarg);
                return false;
            }
            const int bytes = atoi(arg.constData() + colon + 1);
            if (!bytes) {
                fprintf(stderr, "Can't parse -u [%s]\n", optarg);
                return false;
            }
            const Path path = Path::resolved(arg.left(colon));
            if (!path.isFile()) {
                fprintf(stderr, "Can't open [%s] for reading\n", arg.left(colon).nullTerminated());
                return false;
            }

            ByteArray contents(bytes, '\0');
            const int r = fread(contents.data(), 1, bytes, stdin);
            if (r != bytes) {
                fprintf(stderr, "Read error %d (%s). Got %d, expected %d\n",
                        errno, strerror(errno), r, bytes);
            }
            mUnsavedFiles[path] = contents;
            break; }
        case 'f':
        case 'U':
        case 'c':
        case 'r': {
            const ByteArray encoded = Location::encodeClientLocation(optarg);
            if (encoded.isEmpty()) {
                fprintf(stderr, "Can't resolve argument %s\n", optarg);
                return false;
            }
            QueryMessage::Type type = QueryMessage::Invalid;
            switch (c) {
            case 'f': type = QueryMessage::FollowLocation; break;
            case 'U': type = QueryMessage::CursorInfo; break;
            case 'r': type = QueryMessage::ReferencesLocation; break;
            }
            addQuery(type, encoded);
            break; }
        case 'C':
            addQuery(QueryMessage::ClearProjects);
            break;
        case 'g':
            addLog(RdmLogCommand::Default);
            break;
        case 'G':
            addLog(CompilationError);
            break;
        case 'q':
            addQuery(QueryMessage::Shutdown);
            break;
        case 'W':
            addQuery(QueryMessage::DeleteProject, optarg);
            break;
        case 'V':
        case 'w':
        case 'P':
        case 'S':
        case 's': {
            QueryMessage::Type type = QueryMessage::Invalid;
            switch (c) {
            case 'V': type = QueryMessage::Reindex; break;
            case 'w': type = QueryMessage::Project; break;
            case 'P': type = QueryMessage::FindFile; break;
            case 's': type = QueryMessage::Status; break;
            case 'S': type = QueryMessage::ListSymbols; break;
            }

            if (optarg) {
                addQuery(type, optarg);
            } else if (optind < argc && argv[optind][0] != '-') {
                addQuery(type, argv[optind++]);
            } else {
                addQuery(type);
            }
            break; }
        case 't':
            if (optarg) {
                addGRTag(Path::resolved(optarg));
            } else if (optind < argc && argv[optind][0] != '-') {
                addGRTag(Path::resolved(argv[optind++]));
            } else {
                addGRTag(Path::resolved("."));
            }
            break;
        case 'T':
        case 'x':
        case 'd':
        case 'Q': {
            const Path p = Path::resolved(optarg);
            if (!p.isFile()) {
                fprintf(stderr, "%s is not a file\n", optarg);
                return false;
            }
            QueryMessage::Type type = QueryMessage::Invalid;
            switch (c) {
            case 'T': type = QueryMessage::Test; break;
            case 'x': type = QueryMessage::FixIts; break;
            case 'Q': type = QueryMessage::Errors; break;
            case 'd': type = QueryMessage::DumpFile; break;
            }

            addQuery(type, p);
            break; }
        case 'm': {
            Path makefile;
            if (optarg) {
                makefile = Path::resolved(optarg);
                if (!makefile.isFile()) {
                    fprintf(stderr, "%s is not a file\n", optarg);
                    return false;
                }
            } else {
                if (optind < argc) {
                    makefile = Path::resolved(argv[optind]);
                    if (!makefile.isFile()) {
                        makefile = Path::resolved("Makefile");
                        if (!makefile.isFile()) {
                            fprintf(stderr, "%s is not a file", argv[optind]);
                            return false;
                        }
                    } else {
                        ++optind;
                    }
                } else {
                    makefile = Path::resolved("Makefile");
                    if (!makefile.isFile()) {
                        fprintf(stderr, "Can't find a Makefile here\n");
                        return false;
                    }
                }
            }

            List<ByteArray> makefileArgs;
            while (optind < argc && argv[optind][0] != '-')
                makefileArgs.append(argv[optind++]);
            addMakeFile(makefile, makefileArgs);
            break; }
        case 'R':
            addQuery(QueryMessage::ReferencesName, optarg);
            break;
        case 'F':
            addQuery(QueryMessage::FindSymbols, optarg);
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
        return false;
    }

    if (!initLogging(mLogLevel, logFile, logFlags)) {
        fprintf(stderr, "Can't initialize logging with %d %s 0x%0x\n",
                mLogLevel, logFile.constData(), logFlags);
        return false;
    }

    if (rCommands.isEmpty() && !(mClientFlags & (Client::RestartRdm|Client::AutostartRdm))) {
        help(stderr, argv[0]);
        return false;
    }

    if (!logFile.isEmpty() || mLogLevel > 0) {
        Log l(1);
        l << argc;
        for (int i = 0; i < argc; ++i)
            l << " " << argv[i];
    }
    return true;
}
