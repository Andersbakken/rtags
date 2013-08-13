#include "RClient.h"
#include "CompileMessage.h"
#include "CompletionMessage.h"
#include "CreateOutputMessage.h"
#include <rct/Connection.h>
#include <rct/EventLoop.h>
#include <rct/Rct.h>
#include <rct/RegExp.h>

enum OptionType {
    None = 0,
    AbsolutePath,
    AllReferences,
    Builds,
    Clear,
    CodeComplete,
    CodeCompleteAt,
    CodeCompletionEnabled,
    Compile,
    ConnectTimeout,
    ContainingFunction,
    Context,
    CursorInfo,
    CursorInfoIncludeParents,
    CursorInfoIncludeReferences,
    CursorInfoIncludeTargets,
    DeclarationOnly,
    DeleteProject,
    Dependencies,
    Diagnostics,
    DumpFile,
    ElispList,
    FilterSystemHeaders,
    FindFile,
    FindFilePreferExact,
    FindProjectRoot,
    FindSymbols,
    FindVirtuals,
    FixIts,
    FollowLocation,
    HasFileManager,
    Help,
    IMenu,
    IsIndexed,
    IsIndexing,
    JSON,
    JobCount,
    LineNumbers,
    ListSymbols,
    LoadCompilationDatabase,
    LogFile,
    Man,
    MatchCaseInsensitive,
    MatchRegexp,
    Max,
    NoContext,
    PathFilter,
    PreprocessFile,
    Project,
    QuitRdm,
    RangeFilter,
    RdmLog,
    ReferenceLocation,
    ReferenceName,
    Reindex,
    ReloadFileManager,
    ReloadProjects,
    RemoveFile,
    ReverseSort,
    Silent,
    SocketFile,
    Status,
    StripParen,
    Timeout,
    UnloadProject,
    UnsavedFile,
    Verbose,
    WithProject,
    XmlDiagnostics
};

struct Option {
    const OptionType option;
    const char *longOpt;
    const char shortOpt;
    const int argument;
    const char *description;
};

#define DEFAULT_CONNECT_TIMEOUT 1000
#define XSTR(s) #s
#define STR(s) XSTR(s)

struct Option opts[] = {
    { None, 0, 0, 0, "Options:" },
    { Verbose, "verbose", 'v', no_argument, "Be more verbose." },
    { Silent, "silent", 'Q', no_argument, "Be silent." },
    { Help, "help", 'h', no_argument, "Display this help." },

    { None, 0, 0, 0, "" },
    { None, 0, 0, 0, "Rdm:" },
    { QuitRdm, "quit-rdm", 'q', no_argument, "Tell server to shut down." },
    { ConnectTimeout, "connect-timeout", 0, required_argument, "Timeout for connecting to rdm in ms (default " STR(DEFAULT_CONNECT_TIMEOUT)  ")." },

    { None, 0, 0, 0, "" },
    { None, 0, 0, 0, "Project management:" },
    { Clear, "clear", 'C', no_argument, "Clear projects." },
    { Project, "project", 'w', optional_argument, "With arg, select project matching that if unique, otherwise list all projects." },
    { DeleteProject, "delete-project", 'W', required_argument, "Delete all projects matching regexp." },
    { UnloadProject, "unload", 'u', required_argument, "Unload project(s) matching argument." },
    { ReloadProjects, "reload-projects", 'z', no_argument, "Reload projects from projects file." },
    { JobCount, "jobcount", 'j', optional_argument, "Set or query current job count." },

    { None, 0, 0, 0, "" },
    { None, 0, 0, 0, "Commands:" },
    { FollowLocation, "follow-location", 'f', required_argument, "Follow this location." },
    { ReferenceName, "references-name", 'R', required_argument, "Find references matching arg." },
    { ReferenceLocation, "references", 'r', required_argument, "Find references matching this location." },
    { ListSymbols, "list-symbols", 'S', optional_argument, "List symbol names matching arg." },
    { FindSymbols, "find-symbols", 'F', required_argument, "Find symbols matching arg." },
    { CursorInfo, "cursor-info", 'U', required_argument, "Get cursor info for this location." },
    { Status, "status", 's', optional_argument, "Dump status of rdm. Arg can be symbols or symbolNames." },
    { IsIndexed, "is-indexed", 'T', required_argument, "Check if rtags knows about, and is ready to return information about, this source file." },
    { IsIndexing, "is-indexing", 0, no_argument, "Check if rtags is currently indexing files." },
    { HasFileManager, "has-filemanager", 0, optional_argument, "Check if rtags has info about files in this directory." },
    { PreprocessFile, "preprocess", 'E', required_argument, "Preprocess file." },
    { Reindex, "reindex", 'V', optional_argument, "Reindex all files or all files matching pattern." },
    { FindFile, "path", 'P', optional_argument, "Print files matching pattern." },
    { DumpFile, "dump-file", 'd', required_argument, "Dump source file." },
    { RdmLog, "rdm-log", 'g', no_argument, "Receive logs from rdm." },
    { CodeCompleteAt, "code-complete-at", 'x', required_argument, "Get code completion from location (must be specified with path:line:column)." },
    { CodeComplete, "code-complete", 0, no_argument, "Get code completion from stream written to stdin." },
    { FixIts, "fixits", 0, required_argument, "Get fixits for file." },
    { Compile, "compile", 'c', required_argument, "Pass compilation arguments to rdm." },
    { RemoveFile, "remove", 'D', required_argument, "Remove file from project." },
    { FindProjectRoot, "find-project-root", 0, required_argument, "Use to check behavior of find-project-root." },
    { JSON, "json", 0, optional_argument, "Dump json about files matching arg or whole project if no argument." },
    { Builds, "builds", 0, optional_argument, "Dump builds for source file." },
    { Dependencies, "dependencies", 0, required_argument, "Dump dependencies for source file." },
    { ReloadFileManager, "reload-file-manager", 'B', no_argument, "Reload file manager." },
    { Man, "man", 0, no_argument, "Output XML for xmltoman to generate man page for rc :-)" },
    { CodeCompletionEnabled, "code-completion-enabled", 0, no_argument, "Whether completion is enabled." },

    { None, 0, 0, 0, "" },
    { None, 0, 0, 0, "Command flags:" },
    { StripParen, "strip-paren", 'p', no_argument, "Strip parens in various contexts." },
    { Max, "max", 'M', required_argument, "Max lines of output for queries." },
    { ReverseSort, "reverse-sort", 'O', no_argument, "Sort output reversed." },
    { UnsavedFile, "unsaved-file", 0, required_argument, "Pass unsaved file on command line. E.g. --unsaved-file=main.cpp:1200 then write 1200 bytes on stdin." },
    { LogFile, "log-file", 'L', required_argument, "Log to this file." },
    { NoContext, "no-context", 'N', no_argument, "Don't print context for locations." },
    { LineNumbers, "line-numbers", 'l', no_argument, "Output line numbers instead of offsets." },
    { PathFilter, "path-filter", 'i', required_argument, "Filter out results not matching with arg." },
    { RangeFilter, "range-filter", 0, required_argument, "Filter out results not in the specified range." },
    { FilterSystemHeaders, "filter-system-headers", 'H', no_argument, "Don't exempt system headers from path filters." },
    { AllReferences, "all-references", 'e', no_argument, "Include definitions/declarations/constructors/destructors for references. Used for rename symbol." },
    { ElispList, "elisp-list", 'Y', no_argument, "Output elisp: (list \"one\" \"two\" ...)." },
    { Diagnostics, "diagnostics", 'G', no_argument, "Receive continual diagnostics from rdm." },
    { XmlDiagnostics, "xml-diagnostics", 'm', no_argument, "Receive continual XML formatted diagnostics from rdm." },
    { MatchRegexp, "match-regexp", 'Z', no_argument, "Treat various text patterns as regexps (-P, -i, -V)." },
    { MatchCaseInsensitive, "match-icase", 'I', no_argument, "Match case insensitively" },
    { AbsolutePath, "absolute-path", 'K', no_argument, "Print files with absolute path." },
    { SocketFile, "socket-file", 'n', required_argument, "Use this socket file (default ~/.rdm)." },
    { Timeout, "timeout", 'y', required_argument, "Max time in ms to wait for job to finish (default no timeout)." },
    { FindVirtuals, "find-virtuals", 'k', no_argument, "Use in combinations with -R or -r to show other implementations of this function." },
    { FindFilePreferExact, "find-file-prefer-exact", 'A', no_argument, "Use to make --find-file prefer exact matches over partial matches." },
    { CursorInfoIncludeParents, "cursorinfo-include-parents", 0, no_argument, "Use to make --cursor-info include parent cursors." },
    { CursorInfoIncludeTargets, "cursorinfo-include-targets", 0, no_argument, "Use to make --cursor-info include target cursors." },
    { CursorInfoIncludeReferences, "cursorinfo-include-references", 0, no_argument, "Use to make --cursor-info include reference cursors." },
    { WithProject, "with-project", 0, required_argument, "Like --project but pass as a flag." },
    { DeclarationOnly, "declaration-only", 0, no_argument, "Filter out definitions (unless inline).", },
    { IMenu, "imenu", 0, no_argument, "Use with --list-symbols to provide output for (rtags-imenu) (filter namespaces, fully qualified function names, ignore certain cursors etc)." },
    { Context, "context", 't', required_argument, "Context for current symbol (for fuzzy matching with dirty files)." }, // ### multiple context doesn't work
    { ContainingFunction, "containing-function", 'o', no_argument, "Include name of containing function in output. "},
    { LoadCompilationDatabase, "load-compilation-database", 'J', optional_argument, "Load compilation database from JSON file" },
    { None, 0, 0, 0, 0 }
};

static void help(FILE *f, const char* app)
{
    List<String> out;
    int longest = 0;
    for (int i=0; opts[i].description; ++i) {
        if (!opts[i].longOpt && !opts[i].shortOpt) {
            out.append(String());
        } else {
            out.append(String::format<64>("  %s%s%s%s",
                                          opts[i].longOpt ? String::format<4>("--%s", opts[i].longOpt).constData() : "",
                                          opts[i].longOpt && opts[i].shortOpt ? "|" : "",
                                          opts[i].shortOpt ? String::format<2>("-%c", opts[i].shortOpt).constData() : "",
                                          opts[i].argument == required_argument ? " [arg] "
                                          : opts[i].argument == optional_argument ? " [optional] " : ""));
            longest = std::max<int>(out[i].size(), longest);
        }
    }
    fprintf(f, "%s options...\n", app);
    const int count = out.size();
    for (int i=0; i<count; ++i) {
        if (out.at(i).isEmpty()) {
            fprintf(f, "%s\n", opts[i].description);
        } else {
            fprintf(f, "%s%s %s\n",
                    out.at(i).constData(),
                    String(longest - out.at(i).size(), ' ').constData(),
                    opts[i].description);
        }
    }
}

static void man()
{
    String out =
        "<!DOCTYPE manpage SYSTEM \"http://masqmail.cx/xmltoman/xmltoman.dtd\">\n"
        "<?xml-stylesheet type=\"text/xsl\" href=\"http://masqmail.cx/xmltoman/xmltoman.xsl\"?>\n"
        "\n"
        "<manpage name=\"rc\" section=\"1\" desc=\"command line client for RTags\">\n"
        "\n"
        "<synopsis>\n"
        "  <cmd>rc <arg>file.1.xml</arg> > file.1</cmd>\n"
        "</synopsis>\n"
        "\n"
        "<description>\n"
        "\n"
        "<p>rc is a command line client used to control RTags.</p>\n"
        "\n"
        "</description>\n";
    for (int i=0; opts[i].description; ++i) {
        if (*opts[i].description) {
            if (!opts[i].longOpt && !opts[i].shortOpt) {
                if (i)
                    out.append("</section>\n");
                out.append(String::format<128>("<section name=\"%s\">\n", opts[i].description));
            } else {
                out.append(String::format<64>("  <option>%s%s%s%s<optdesc>%s</optdesc></option>\n",
                                              opts[i].longOpt ? String::format<4>("--%s", opts[i].longOpt).constData() : "",
                                              opts[i].longOpt && opts[i].shortOpt ? "|" : "",
                                              opts[i].shortOpt ? String::format<2>("-%c", opts[i].shortOpt).constData() : "",
                                              opts[i].argument == required_argument ? " [arg] "
                                              : opts[i].argument == optional_argument ? " [optional] " : "",
                                              opts[i].description));
            }
        }
    }
    out.append("</section>\n"
               "<section name=\"Authors\">\n"
               "  <p>RTags was written by Jan Erik Hanssen &lt;jhanssen@gmail.com&gt; and Anders Bakken &lt;abakken@gmail.com&gt;</p>\n"
               "</section>\n"
               "<section name=\"See also\">\n"
               "  <p><manref name=\"rdm\" section=\"1\"/></p>\n"
               "</section>\n"
               "<section name=\"Comments\">\n"
               "  <p>This man page was written using <manref name=\"xmltoman\" section=\"1\" href=\"http://masqmail.cx/xmltoman/\"/>.</p>\n"
               "</section>\n"
               "</manpage>\n");
    printf("%s", out.constData());
}

class RCCommand
{
public:
    RCCommand(unsigned int f)
        : flags(f)
    {}
    virtual ~RCCommand() {}
    enum Flag {
        None = 0x0,
        RequiresNon0Output = 0x1
    };
    virtual bool exec(RClient *rc, Connection *connection) = 0;
    virtual String description() const = 0;

    const unsigned int flags;
};

class QueryCommand : public RCCommand
{
public:
    QueryCommand(QueryMessage::Type t, const String &q, unsigned int flags)
        : RCCommand(flags), type(t), query(q), extraQueryFlags(0)
    {}

    const QueryMessage::Type type;
    const String query;
    unsigned extraQueryFlags;

    virtual bool exec(RClient *rc, Connection *connection)
    {
        QueryMessage msg(type);
        msg.init(rc->argc(), rc->argv());
        msg.setQuery(query);
        msg.setContext(rc->context());
        msg.setFlags(extraQueryFlags | rc->queryFlags());
        msg.setMax(rc->max());
        msg.setPathFilters(rc->pathFilters().toList());
        msg.setRangeFilter(rc->minOffset(), rc->maxOffset());
        msg.setProjects(rc->projects());
        return connection->send(&msg);
    }

    virtual String description() const
    {
        return ("QueryMessage " + String::number(type) + " " + query);
    }
};

class CompletionCommand : public RCCommand
{
public:
    CompletionCommand(const Path &p, int l, int c)
        : RCCommand(0), path(p), line(l), column(c), stream(false), connection(0)
    {}
    CompletionCommand()
        : RCCommand(0), line(-1), column(-1), stream(true), connection(0)
    {
    }

    const Path path;
    const int line;
    const int column;
    const bool stream;
    Connection *connection;
    String data;

    virtual bool exec(RClient *rc, Connection *cl)
    {
        connection = cl;
        if (stream) {
            CompletionMessage msg(CompletionMessage::Stream);
            msg.init(rc->argc(), rc->argv());
            msg.setProjects(rc->projects());
            EventLoop::eventLoop()->registerSocket(STDIN_FILENO, EventLoop::SocketRead, std::bind(&CompletionCommand::processStdin, this));
            return connection->send(&msg);
        } else {
            CompletionMessage msg(CompletionMessage::None, path, line, column);
            msg.init(rc->argc(), rc->argv());
            msg.setContents(rc->unsavedFiles().value(path));
            msg.setProjects(rc->projects());
            return connection->send(&msg);
        }
    }

    virtual String description() const
    {
        return String::format<128>("CompletionMessage %s:%d:%d", path.constData(), line, column);
    }

    void processStdin()
    {
        assert(!data.contains('\n'));
        while (true) {
            const int ch = getc(stdin);
            if (ch == EOF)
                return;
            if (ch == '\n')
                break;
            data.append(static_cast<char>(ch));
        }
        const int colon = data.indexOf(':');
        if (colon == -1) {
            error() << "Failed to match completion header" << data;
            EventLoop::eventLoop()->unregisterSocket(STDIN_FILENO);
            EventLoop::eventLoop()->quit();
            return;
        }

        int line, column, contentsSize, pos;
        const int ret = sscanf(data.constData() + colon + 1, "%d:%d:%d:%d", &line, &column, &pos, &contentsSize);
        if (ret != 4) {
            error() << "Failed to match completion header" << ret << "\n" << data;
            EventLoop::eventLoop()->unregisterSocket(STDIN_FILENO);
            EventLoop::eventLoop()->quit();
            return;
        }
        String contents(contentsSize, ' ');
        int read = 0;
        char *c = contents.data();
        while (read < contentsSize) {
            const int r = fread(c + read, sizeof(char), contentsSize - read, stdin);
            if (r < 0) {
                EventLoop::eventLoop()->unregisterSocket(STDIN_FILENO);
                EventLoop::eventLoop()->quit();
                return;
            }
            read += r;
        }
        Path path(data.constData(), colon);
        data.clear();
        if (!path.resolve(Path::MakeAbsolute)) {
            error() << "Can't resolve" << path;
            return;
        }
        // error() << path << line << column << contentsSize << pos << "\n" << contents.left(100)
        //         << contents.right(100);

        CompletionMessage msg(CompletionMessage::None, path, line, column, pos);
        const String args = String::format<64>("%s:%d:%d:%d:%d", path.constData(), line, column, pos, contentsSize);
        const char *argv[] = { "completionStream", args.constData() };
        msg.init(2, argv);
        msg.setContents(contents);
        connection->send(&msg);
    }
};

class RdmLogCommand : public RCCommand
{
public:
    enum { Default = -3 };

    RdmLogCommand(int level)
        : RCCommand(0), mLevel(level)
    {
    }
    virtual bool exec(RClient *rc, Connection *connection)
    {
        CreateOutputMessage msg(mLevel == Default ? rc->logLevel() : mLevel);
        msg.init(rc->argc(), rc->argv());
        return connection->send(&msg);
    }
    virtual String description() const
    {
        return "RdmLogCommand";
    }
    const int mLevel;
};

class CompileCommand : public RCCommand
{
public:
    CompileCommand(const Path &c, const String &a)
        : RCCommand(0), cwd(c), args(a)
    {}
    const Path cwd;
    const String args;
    virtual bool exec(RClient *rc, Connection *connection)
    {
        CompileMessage msg(cwd, args);
        msg.init(rc->argc(), rc->argv());
        msg.setProjects(rc->projects());
        return connection->send(&msg);
    }
    virtual String description() const
    {
        return ("CompileMessage " + cwd);
    }
};

RClient::RClient()
    : mQueryFlags(0), mMax(-1), mLogLevel(0), mTimeout(-1),
      mMinOffset(-1), mMaxOffset(-1), mConnectTimeout(DEFAULT_CONNECT_TIMEOUT), mArgc(0), mArgv(0)
{
}

RClient::~RClient()
{
    cleanupLogging();
}

void RClient::addQuery(QueryMessage::Type t, const String &query)
{
    unsigned int flags = RCCommand::None;
    unsigned int extraQueryFlags = 0;
    switch (t) {
    case QueryMessage::CodeCompletionEnabled:
    case QueryMessage::IsIndexing:
    case QueryMessage::HasFileManager:
        flags |= RCCommand::RequiresNon0Output;
        break;
    case QueryMessage::FindFile:
        extraQueryFlags |= QueryMessage::WaitForLoadProject;
    default:
        break;
    }
    shared_ptr<QueryCommand> cmd(new QueryCommand(t, query, flags));
    cmd->extraQueryFlags = extraQueryFlags;
    mCommands.append(cmd);
}

void RClient::addLog(int level)
{
    mCommands.append(shared_ptr<RCCommand>(new RdmLogCommand(level)));
}

void RClient::addCompile(const Path &cwd, const String &args)
{
    mCommands.append(shared_ptr<RCCommand>(new CompileCommand(cwd, args)));
}

class LogMonitor : public LogOutput
{
public:
    LogMonitor()
        : LogOutput(Error), gotNon0Output(false)
    {}

    virtual void log(const char *msg, int len)
    {
        if (!gotNon0Output && len && msg && (len > 1 || *msg != '0'))
            gotNon0Output = true;
    }

    bool gotNon0Output;
};

bool RClient::exec()
{
    bool ret = true;
    RTags::initMessages();

    EventLoop::SharedPtr loop(new EventLoop);
    loop->init(EventLoop::MainEventLoop);
    LogMonitor monitor;

    const int commandCount = mCommands.size();
    bool requiresNon0Output = false;
    for (int i=0; i<commandCount; ++i) {
        Connection connection;
        if (!connection.connectToServer(mSocketFile, mConnectTimeout)) {
            error("Can't seem to connect to server");
            ret = false;
            break;
        }

        connection.newMessage().connect(std::bind(&RClient::onNewMessage, this,
                                                  std::placeholders::_1, std::placeholders::_2));
        connection.disconnected().connect(std::bind([](){ EventLoop::eventLoop()->quit(); }));

        const shared_ptr<RCCommand> &cmd = mCommands.at(i);
        requiresNon0Output = cmd->flags & RCCommand::RequiresNon0Output;
        debug() << "running command " << cmd->description();
        ret = cmd->exec(this, &connection) && loop->exec(timeout()) == 0;
        if (!ret)
            break;
    }
    mCommands.clear();
    ret = ret && (!requiresNon0Output || monitor.gotNon0Output);
    return ret;
}

bool RClient::parse(int &argc, char **argv)
{
    Rct::findExecutablePath(*argv);
    mSocketFile = Path::home() + ".rdm";

    List<option> options;
    options.reserve(sizeof(opts) / sizeof(Option));
    List<std::shared_ptr<QueryCommand> > projectCommands;

    String shortOptionString;
    Map<int, Option*> shortOptions, longOptions;
    for (int i=0; opts[i].description; ++i) {
        if (opts[i].option != None) {
            const option opt = { opts[i].longOpt, opts[i].argument, 0, opts[i].shortOpt };
            if (opts[i].shortOpt) {
                shortOptionString.append(opts[i].shortOpt);
                switch (opts[i].argument) {
                case no_argument:
                    break;
                case required_argument:
                    shortOptionString.append(':');
                    break;
                case optional_argument:
                    shortOptionString.append("::");
                    break;
                }
                assert(!shortOptions.contains(opts[i].shortOpt));
                shortOptions[opts[i].shortOpt] = &opts[i];
            }
            if (opts[i].longOpt)
                longOptions[options.size()] = &opts[i];
            options.push_back(opt);
        }
    }

    if (getenv("RTAGS_DUMP_UNUSED")) {
        String unused;
        for (int i=0; i<26; ++i) {
            if (!shortOptionString.contains('a' + i))
                unused.append('a' + i);
            if (!shortOptionString.contains('A' + i))
                unused.append('A' + i);
        }
        printf("Unused: %s\n", unused.constData());
        for (int i=0; opts[i].description; ++i) {
            if (opts[i].longOpt) {
                if (!opts[i].shortOpt) {
                    printf("No shortoption for %s\n", opts[i].longOpt);
                } else if (opts[i].longOpt[0] != opts[i].shortOpt) {
                    printf("Not ideal option for %s|%c\n", opts[i].longOpt, opts[i].shortOpt);
                }
            }
        }
        return 0;
    }

    {
        const option opt = { 0, 0, 0, 0 };
        options.push_back(opt);
    }

    Path logFile;
    unsigned logFlags = 0;

    enum State {
        Parsing,
        Done,
        Error
    } state = Parsing;
    while (true) {
        int idx = -1;
        const int c = getopt_long(argc, argv, shortOptionString.constData(), options.data(), &idx);
        switch (c) {
        case -1:
            state = Done;
            break;
        case '?':
        case ':':
            state = Error;
            break;
        default:
            break;
        }
        if (state != Parsing)
            break;

        const Option *opt = (idx == -1 ? shortOptions.value(c) : longOptions.value(idx));
        assert(opt);

        switch (opt->option) {
        case None:
            assert(0);
            break;
        case Help:
            help(stdout, argv[0]);
            return 0;
        case Man:
            man();
            return 0;
        case SocketFile:
            mSocketFile = optarg;
            break;
        case IMenu:
            mQueryFlags |= QueryMessage::IMenu;
            break;
        case ContainingFunction:
            mQueryFlags |= QueryMessage::ContainingFunction;
            break;
        case DeclarationOnly:
            mQueryFlags |= QueryMessage::DeclarationOnly;
            break;
        case FindVirtuals:
            mQueryFlags |= QueryMessage::FindVirtuals;
            break;
        case FindFilePreferExact:
            mQueryFlags |= QueryMessage::FindFilePreferExact;
            break;
        case CursorInfoIncludeParents:
            mQueryFlags |= QueryMessage::CursorInfoIncludeParents;
            break;
        case CursorInfoIncludeTargets:
            mQueryFlags |= QueryMessage::CursorInfoIncludeTargets;
            break;
        case CursorInfoIncludeReferences:
            mQueryFlags |= QueryMessage::CursorInfoIncludeReferences;
            break;
        case CodeComplete:
            // logFile = "/tmp/rc.log";
            mCommands.append(shared_ptr<RCCommand>(new CompletionCommand));
            break;
        case Context:
            mContext = optarg;
            break;
        case CodeCompleteAt: {
            const String arg = optarg;
            List<RegExp::Capture> caps;
            RegExp rx("^\\(.*\\):\\([0-9][0-9]*\\):\\([0-9][0-9]*\\)$");
            if (rx.indexIn(arg, 0, &caps) != 0 || caps.size() != 4) {
                fprintf(stderr, "Can't decode argument for --code-complete-at [%s]\n", optarg);
                return false;
            }
            const Path path = Path::resolved(caps[1].capture, Path::MakeAbsolute);
            if (!path.exists()) {
                fprintf(stderr, "Can't decode argument for --code-complete-at [%s]\n", optarg);
                return false;
            }

            String out;
            {
                Serializer serializer(out);
                serializer << path << atoi(caps[2].capture.constData()) << atoi(caps[3].capture.constData());
            }
            CompletionCommand *cmd = new CompletionCommand(path, atoi(caps[2].capture.constData()), atoi(caps[3].capture.constData()));
            mCommands.append(shared_ptr<RCCommand>(cmd));
            break; }
        case AllReferences:
            mQueryFlags |= QueryMessage::AllReferences;
            break;
        case MatchCaseInsensitive:
            mQueryFlags |= QueryMessage::MatchCaseInsensitive;
            break;
        case MatchRegexp:
            mQueryFlags |= QueryMessage::MatchRegexp;
            break;
        case AbsolutePath:
            mQueryFlags |= QueryMessage::AbsolutePath;
            break;
        case ReverseSort:
            mQueryFlags |= QueryMessage::ReverseSort;
            break;
        case ElispList:
            mQueryFlags |= QueryMessage::ElispList;
            break;
        case FilterSystemHeaders:
            mQueryFlags |= QueryMessage::FilterSystemIncludes;
            break;
        case NoContext:
            mQueryFlags |= QueryMessage::NoContext;
            break;
        case PathFilter:
            mPathFilters.insert(optarg);
            break;
        case RangeFilter: {
            List<RegExp::Capture> caps;
            RegExp rx("^\\([0-9][0-9]*\\)-\\([0-9][0-9]*\\)$");
            if (rx.indexIn(optarg, 0, &caps) != 0 || caps.size() != 3) {
                fprintf(stderr, "Can't parse range, must be uint-uint. E.g. 1-123\n");
                return false;
            } else {
                mMinOffset = atoi(caps.at(1).capture.constData());
                mMaxOffset = atoi(caps.at(2).capture.constData());
                if (mMaxOffset <= mMinOffset || mMinOffset < 0) {
                    fprintf(stderr, "Invalid range (%d-%d), must be uint-uint. E.g. 1-123\n", mMinOffset, mMaxOffset);
                    return false;
                }
            }
            break; }
        case LineNumbers:
            mQueryFlags |= QueryMessage::LineNumbers;
            break;
        case Verbose:
            ++mLogLevel;
            break;
        case Silent:
            mLogLevel = -1;
            break;
        case LogFile:
            logFile = optarg;
            break;
        case StripParen:
            mQueryFlags |= QueryMessage::StripParentheses;
            break;
        case ConnectTimeout:
            mConnectTimeout = atoi(optarg);
            if (mConnectTimeout < 0) {
                fprintf(stderr, "--connect-timeout [arg] must be >= 0\n");
                return false;
            }
            break;
        case Max:
            mMax = atoi(optarg);
            if (mMax <= 0) {
                fprintf(stderr, "-M [arg] must be positive integer\n");
                return false;
            }
            break;
        case Timeout:
            mTimeout = atoi(optarg);
            if (mTimeout <= 0) {
                fprintf(stderr, "-y [arg] must be positive integer\n");
                return false;
            }
            break;
        case UnsavedFile: {
            const String arg(optarg);
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
            const Path path = Path::resolved(arg.left(colon), Path::MakeAbsolute);
            if (!path.isFile()) {
                fprintf(stderr, "Can't open [%s] for reading\n", arg.left(colon).nullTerminated());
                return false;
            }

            String contents(bytes, '\0');
            const int r = fread(contents.data(), 1, bytes, stdin);
            if (r != bytes) {
                fprintf(stderr, "Read error %d (%s). Got %d, expected %d\n",
                        errno, strerror(errno), r, bytes);
                return false;
            }
            mUnsavedFiles[path] = contents;
            break; }
        case FollowLocation:
        case CursorInfo:
        case ReferenceLocation: {
            const String encoded = Location::encodeClientLocation(optarg);
            if (encoded.isEmpty()) {
                fprintf(stderr, "Can't resolve argument %s\n", optarg);
                return false;
            }
            QueryMessage::Type type = QueryMessage::Invalid;
            switch (opt->option) {
            case FollowLocation: type = QueryMessage::FollowLocation; break;
            case CursorInfo: type = QueryMessage::CursorInfo; break;
            case ReferenceLocation: type = QueryMessage::ReferencesLocation; break;
            default: assert(0); break;
            }
            addQuery(type, encoded);
            break; }
        case WithProject:
            mProjects.append(optarg);
            break;
        case ReloadFileManager:
            addQuery(QueryMessage::ReloadFileManager);
            break;
        case ReloadProjects:
            addQuery(QueryMessage::ReloadProjects);
            break;
        case Clear:
            addQuery(QueryMessage::ClearProjects);
            break;
        case RdmLog:
            addLog(RdmLogCommand::Default);
            break;
        case Diagnostics:
            addLog(RTags::CompilationError);
            break;
        case XmlDiagnostics:
            addLog(RTags::CompilationErrorXml);
            break;
        case QuitRdm:
            addQuery(QueryMessage::Shutdown);
            break;
        case DeleteProject:
            addQuery(QueryMessage::DeleteProject, optarg);
            break;
        case CodeCompletionEnabled:
            addQuery(QueryMessage::CodeCompletionEnabled);
            break;
        case UnloadProject:
            addQuery(QueryMessage::UnloadProject, optarg);
            break;
        case FindProjectRoot: {
            const Path p = Path::resolved(optarg);
            printf("findProjectRoot [%s] => [%s]\n", p.constData(),
                   RTags::findProjectRoot(p).constData());
            return 0; }
        case Reindex:
        case Project:
        case FindFile:
        case ListSymbols:
        case JSON:
        case Builds:
        case JobCount:
        case Status: {
            QueryMessage::Type type = QueryMessage::Invalid;
            switch (opt->option) {
            case Reindex: type = QueryMessage::Reindex; break;
            case Project: type = QueryMessage::Project; break;
            case FindFile: type = QueryMessage::FindFile; break;
            case Builds: type = QueryMessage::Builds; break;
            case Status: type = QueryMessage::Status; break;
            case JSON: type = QueryMessage::JSON; break;
            case ListSymbols: type = QueryMessage::ListSymbols; break;
            case JobCount: type = QueryMessage::JobCount; break;
            default: assert(0); break;
            }

            if (optarg) {
                addQuery(type, optarg);
            } else if (optind < argc && argv[optind][0] != '-') {
                addQuery(type, argv[optind++]);
            } else {
                addQuery(type);
            }
            assert(!mCommands.isEmpty());
            if (type == QueryMessage::Project)
                projectCommands.append(std::static_pointer_cast<QueryCommand>(mCommands.back()));
            break; }
        case LoadCompilationDatabase: {
            Path fileName;
            if (optarg) {
                fileName = optarg;
            } else if (optind < argc && argv[optind][0] != '-') {
                fileName = argv[optind++];
            } else {
                fileName = "compile_commands.json";
            }
            fileName.resolve(Path::MakeAbsolute);
            if (!fileName.exists()) {
                fprintf(stderr, "%s does not seem to exist\n", fileName.constData());
                return false;
            }
            addQuery(QueryMessage::LoadCompilationDatabase, fileName);
            break; }
        case HasFileManager: {
            Path p;
            if (optarg) {
                p = optarg;
            } else if (optind < argc && argv[optind][0] != '-') {
                p = argv[optind++];
            } else {
                p = ".";
            }
            p.resolve(Path::MakeAbsolute);
            if (!p.exists()) {
                fprintf(stderr, "%s does not seem to exist\n", optarg);
                return false;
            }
            if (p.isDir())
                p.append('/');
            addQuery(QueryMessage::HasFileManager, p);
            break; }
        case Compile: {
            String args = optarg;
            while (optind < argc) {
                args.append(' ');
                args.append(argv[optind++]);
            }
            addCompile(Path::pwd(), args);
            break; }
        case IsIndexing:
            addQuery(QueryMessage::IsIndexing);
            break;
        case IsIndexed:
        case DumpFile:
        case Dependencies:
        case FixIts: {
            Path p = optarg;
            if (!p.exists()) {
                fprintf(stderr, "%s does not exist\n", optarg);
                return false;
            }

            if (!p.isAbsolute())
                p.prepend(Path::pwd());

            if (p.isDir()) {
                if (opt->option != IsIndexed) {
                    fprintf(stderr, "%s is not a file\n", optarg);
                    return false;
                } else if (!p.endsWith('/')) {
                    p.append('/');
                }
            }
            QueryMessage::Type type = QueryMessage::Invalid;
            switch (opt->option) {
            case Dependencies: type = QueryMessage::Dependencies; break;
            case FixIts: type = QueryMessage::FixIts; break;
            case IsIndexed: type = QueryMessage::IsIndexed; break;
            case DumpFile: type = QueryMessage::DumpFile; break;
            default: assert(0); break;
            }

            addQuery(type, p);
            break; }
        case PreprocessFile: {
            unsigned long long idx = 0;
            Path p = optarg;
            if (!p.exists()) {
                List<RegExp::Capture> caps;
                const RegExp rx(".*_\\([0-9][0-9]*\\)$");
                const int match = rx.indexIn(p, 0, &caps);
                if (match != -1) {
                    assert(caps.size() == 2);
                    idx = caps.at(1).capture.toULongLong();
                    if (idx > 255) {
                        fprintf(stderr, "Invalid build index %llu (must be < 256)\n", idx);
                        return false;
                    }
                    p.resize(caps.at(1).index - 1);
                }
            }
            p.resolve(Path::MakeAbsolute);
            if (!p.isFile()) {
                fprintf(stderr, "%s is not a file\n", optarg);
                return false;
            }
            addQuery(QueryMessage::PreprocessFile, p);
            break; }

        case RemoveFile: {
            const Path p = Path::resolved(optarg, Path::MakeAbsolute);
            if (!p.exists()) {
                addQuery(QueryMessage::RemoveFile, p);
            } else {
                addQuery(QueryMessage::RemoveFile, optarg);
            }
            break; }
        case ReferenceName:
            addQuery(QueryMessage::ReferencesName, optarg);
            break;
        case FindSymbols:
            addQuery(QueryMessage::FindSymbols, optarg);
            break;
        }
    }
    if (state == Error) {
        help(stderr, argv[0]);
        return false;
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


    if (mCommands.isEmpty()) {
        help(stderr, argv[0]);
        return false;
    }
    if (mCommands.size() > projectCommands.size()) {
        // If there's more than one command one likely does not want output from
        // the queryCommand (unless there's no arg specified for it). This is so
        // we don't have to pass a different flag for auto-updating project
        // using the current buffer but rather piggy-back on --project
        const int count = projectCommands.size();
        for (int i=0; i<count; ++i) {
            shared_ptr<QueryCommand> &cmd = projectCommands[i];
            if (!cmd->query.isEmpty()) {
                cmd->extraQueryFlags |= QueryMessage::Silent;
            }
        }
    }

    if (!logFile.isEmpty() || mLogLevel > 0) {
        Log l(1);
        l << argc;
        for (int i = 0; i < argc; ++i)
            l << " " << argv[i];
    }
    mArgc = argc;
    mArgv = argv;

    return true;
}

void RClient::onNewMessage(const Message *message, Connection *)
{
    if (message->messageId() == ResponseMessage::MessageId) {
        const String response = static_cast<const ResponseMessage*>(message)->data();
        if (!response.isEmpty()) {
            error("%s", response.constData());
            fflush(stdout);
        }
    } else {
        error("Unexpected message: %d", message->messageId());
    }
}
