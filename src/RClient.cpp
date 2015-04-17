/* This file is part of RTags (http://rtags.net).

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

#include "RClient.h"
#include "IndexMessage.h"
#include "LogOutputMessage.h"
#include "RTagsLogOutput.h"
#include <rct/Connection.h>
#include <rct/EventLoop.h>
#include <rct/Log.h>
#include <rct/QuitMessage.h>
#include <rct/Rct.h>
#include "RTagsClang.h"
#include "FileMap.h"
#include <rct/StopWatch.h>
#include <regex>

struct Option {
    const RClient::OptionType option;
    const char *longOpt;
    const char shortOpt;
    const int argument;
    const char *description;
};

#define DEFAULT_CONNECT_TIMEOUT 1000
#define XSTR(s) #s
#define STR(s) XSTR(s)

struct Option opts[] = {
    { RClient::None, 0, 0, 0, "Options:" },
    { RClient::Verbose, "verbose", 'v', no_argument, "Be more verbose." },
    { RClient::Version, "version", 0, no_argument, "Print current version." },
    { RClient::Silent, "silent", 'Q', no_argument, "Be silent." },
    { RClient::Help, "help", 'h', no_argument, "Display this help." },

    { RClient::None, 0, 0, 0, "" },
    { RClient::None, 0, 0, 0, "Rdm:" },
    { RClient::QuitRdm, "quit-rdm", 'q', no_argument, "Tell server to shut down with optional exit code as argument." },
    { RClient::ConnectTimeout, "connect-timeout", 0, required_argument, "Timeout for connecting to rdm in ms (default " STR(DEFAULT_CONNECT_TIMEOUT)  ")." },

    { RClient::None, 0, 0, 0, "" },
    { RClient::None, 0, 0, 0, "Project management:" },
    { RClient::Clear, "clear", 'C', no_argument, "Clear projects." },
    { RClient::Project, "project", 'w', optional_argument, "With arg, select project matching that if unique, otherwise list all projects." },
    { RClient::DeleteProject, "delete-project", 'W', required_argument, "Delete all projects matching regex." },
    { RClient::ReloadProjects, "reload-projects", 'z', no_argument, "Reload projects from projects file." },
    { RClient::JobCount, "jobcount", 'j', optional_argument, "Set or query current job count. (Prefix with l to set low-priority-job-count)." },

    { RClient::None, 0, 0, 0, "" },
    { RClient::None, 0, 0, 0, "Indexing commands:" },
    { RClient::Compile, "compile", 'c', optional_argument, "Pass compilation arguments to rdm." },
    { RClient::GuessFlags, "guess-flags", 0, no_argument, "Guess compile flags (used with -c)." },
#if CLANG_VERSION_MAJOR > 3 || (CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR > 3)
    { RClient::LoadCompilationDatabase, "load-compilation-database", 'J', optional_argument, "Load compile_commands.json from directory" },
#endif
    { RClient::Suspend, "suspend", 'X', optional_argument, "Dump suspended files (don't track changes in these files) with no arg. Otherwise toggle suspension for arg." },

    { RClient::None, 0, 0, 0, "" },
    { RClient::None, 0, 0, 0, "Query commands:" },
    { RClient::FollowLocation, "follow-location", 'f', required_argument, "Follow this location." },
    { RClient::ReferenceName, "references-name", 'R', required_argument, "Find references matching arg." },
    { RClient::ReferenceLocation, "references", 'r', required_argument, "Find references matching this location." },
    { RClient::ListSymbols, "list-symbols", 'S', optional_argument, "List symbol names matching arg." },
    { RClient::FindSymbols, "find-symbols", 'F', optional_argument, "Find symbols matching arg." },
    { RClient::SymbolInfo, "symbol-info", 'U', required_argument, "Get cursor info for this location." },
    { RClient::Status, "status", 's', optional_argument, "Dump status of rdm. Arg can be symbols or symbolNames." },
    { RClient::IsIndexed, "is-indexed", 'T', required_argument, "Check if rtags knows about, and is ready to return information about, this source file." },
    { RClient::IsIndexing, "is-indexing", 0, no_argument, "Check if rtags is currently indexing files." },
    { RClient::HasFileManager, "has-filemanager", 0, optional_argument, "Check if rtags has info about files in this directory." },
    { RClient::PreprocessFile, "preprocess", 'E', required_argument, "Preprocess file." },
    { RClient::Reindex, "reindex", 'V', optional_argument, "Reindex all files or all files matching pattern." },
    { RClient::CheckReindex, "check-reindex", 'x', optional_argument, "Check if reindexing is necessary for all files matching pattern." },
    { RClient::FindFile, "path", 'P', optional_argument, "Print files matching pattern." },
    { RClient::DumpFile, "dump-file", 'd', required_argument, "Dump source file." },
    { RClient::GenerateTest, "generate-test", 0, required_argument, "Generate a test for a given source file." },
    { RClient::RdmLog, "rdm-log", 'g', no_argument, "Receive logs from rdm." },
    { RClient::FixIts, "fixits", 0, required_argument, "Get fixits for file." },
    { RClient::RemoveFile, "remove", 'D', required_argument, "Remove file from project." },
    { RClient::FindProjectRoot, "find-project-root", 0, required_argument, "Use to check behavior of find-project-root." },
    { RClient::FindProjectBuildRoot, "find-project-build-root", 0, required_argument, "Use to check behavior of find-project-root for builds." },
    { RClient::IncludeFile, "include-file", 0, required_argument, "Use to generate include statement for symbol." },
    { RClient::Sources, "sources", 0, optional_argument, "Dump sources for source file." },
    { RClient::Dependencies, "dependencies", 0, required_argument, "Dump dependencies for source file." },
    { RClient::ReloadFileManager, "reload-file-manager", 'B', no_argument, "Reload file manager." },
    { RClient::Man, "man", 0, no_argument, "Output XML for xmltoman to generate man page for rc :-)" },
    { RClient::CodeCompleteAt, "code-complete-at", 'l', required_argument, "Code complete at location: arg is file:line:col." },
    { RClient::PrepareCodeCompleteAt, "prepare-code-complete-at", 'b', required_argument, "Prepare code completion at location: arg is file:line:col." },
    { RClient::SendDiagnostics, "send-diagnostics", 0, required_argument, "Only for debugging. Send data to all -G connections." },
    { RClient::DumpCompletions, "dump-completions", 0, no_argument, "Dump cached completions." },
    { RClient::DumpCompilationDatabase, "dump-compilation-database", 0, no_argument, "Dump compilation database for project." },
    { RClient::SetBuffers, "set-buffers", 0, optional_argument, "Set active buffers (list of filenames for active buffers in editor)." },
    { RClient::ListBuffers, "list-buffers", 0, no_argument, "List active buffers." },
    { RClient::ClassHierarchy, "class-hierarchy", 0, required_argument, "Dump class hierarcy for struct/class at location." },

    { RClient::None, 0, 0, 0, "" },
    { RClient::None, 0, 0, 0, "Command flags:" },
    { RClient::StripParen, "strip-paren", 'p', no_argument, "Strip parens in various contexts." },
    { RClient::Max, "max", 'M', required_argument, "Max lines of output for queries." },
    { RClient::ReverseSort, "reverse-sort", 'O', no_argument, "Sort output reversed." },
    { RClient::Rename, "rename", 0, no_argument, "Used for --references to indicate that we're using the results to rename symbols." },
    { RClient::UnsavedFile, "unsaved-file", 0, required_argument, "Pass unsaved file on command line. E.g. --unsaved-file=main.cpp:1200 then write 1200 bytes on stdin." },
    { RClient::LogFile, "log-file", 'L', required_argument, "Log to this file." },
    { RClient::NoContext, "no-context", 'N', no_argument, "Don't print context for locations." },
    { RClient::PathFilter, "path-filter", 'i', required_argument, "Filter out results not matching with arg." },
    { RClient::RangeFilter, "range-filter", 0, required_argument, "Filter out results not in the specified range." },
    { RClient::FilterSystemHeaders, "filter-system-headers", 'H', no_argument, "Don't exempt system headers from path filters." },
    { RClient::AllReferences, "all-references", 'e', no_argument, "Include definitions/declarations/constructors/destructors for references. Used for rename symbol." },
    { RClient::AllTargets, "all-targets", 0, no_argument, "Print all targets for -f. Used for debugging." },
    { RClient::ElispList, "elisp-list", 'Y', no_argument, "Output elisp: (list \"one\" \"two\" ...)." },
    { RClient::Diagnostics, "diagnostics", 'G', no_argument, "Receive continual diagnostics from rdm." },
    { RClient::XmlDiagnostics, "xml-diagnostics", 'm', no_argument, "Receive continual XML formatted diagnostics from rdm." },
    { RClient::MatchRegex, "match-regexp", 'Z', no_argument, "Treat various text patterns as regexps (-P, -i, -V)." },
    { RClient::MatchCaseInsensitive, "match-icase", 'I', no_argument, "Match case insensitively" },
    { RClient::AbsolutePath, "absolute-path", 'K', no_argument, "Print files with absolute path." },
    { RClient::SocketFile, "socket-file", 'n', required_argument, "Use this socket file (default ~/.rdm)." },
    { RClient::Timeout, "timeout", 'y', required_argument, "Max time in ms to wait for job to finish (default no timeout)." },
    { RClient::FindVirtuals, "find-virtuals", 'k', no_argument, "Use in combinations with -R or -r to show other implementations of this function." },
    { RClient::FindFilePreferExact, "find-file-prefer-exact", 'A', no_argument, "Use to make --find-file prefer exact matches over partial matches." },
    { RClient::SymbolInfoIncludeParents, "symbol-info-include-parents", 0, no_argument, "Use to make --symbol-info include parent symbols." },
    { RClient::SymbolInfoExcludeTargets, "symbol-info-exclude-targets", 0, no_argument, "Use to make --symbol-info exclude target symbols." },
    { RClient::SymbolInfoExcludeReferences, "symbol-info-exclude-references", 0, no_argument, "Use to make --symbol-info exclude reference symbols." },
    { RClient::CursorKind, "cursor-kind", 0, no_argument, "Include cursor kind in --find-symbols output." },
    { RClient::DisplayName, "display-name", 0, no_argument, "Include display name in --find-symbols output." },
    { RClient::CurrentFile, "current-file", 0, required_argument, "Pass along which file is being edited to give rdm a better chance at picking the right project." },
    { RClient::DeclarationOnly, "declaration-only", 0, no_argument, "Filter out definitions (unless inline).", },
    { RClient::IMenu, "imenu", 0, no_argument, "Use with --list-symbols to provide output for (rtags-imenu) (filter namespaces, fully qualified function names, ignore certain symbols etc)." },
    { RClient::ContainingFunction, "containing-function", 'o', no_argument, "Include name of containing function in output."},
    { RClient::BuildIndex, "build-index", 0, required_argument, "For sources with multiple builds, use the arg'th." },
    { RClient::CompilationFlagsOnly, "compilation-flags-only", 0, no_argument, "For --source, only print compilation flags." },
    { RClient::CompilationFlagsSplitLine, "compilation-flags-split-line", 0, no_argument, "For --source, print one compilation flag per line." },
    { RClient::DumpIncludeHeaders, "dump-include-headers", 0, no_argument, "For --dump-file, also dump dependencies." },
    { RClient::SilentQuery, "silent-query", 0, no_argument, "Don't log this request in rdm." },
    { RClient::SynchronousCompletions, "synchronous-completions", 0, no_argument, "Wait for completion results." },
    { RClient::UnescapeCompileCommands, "unescape-compile-commands", 0, no_argument, "Unescape \\'s and unquote arguments to -c." },
    { RClient::NoUnescapeCompileCommands, "no-unescape-compile-commands", 0, no_argument, "Escape \\'s and unquote arguments to -c." },
    { RClient::NoSortReferencesByInput, "no-sort-references-by-input", 0, no_argument, "Don't sort references by input position." },
    { RClient::ProjectRoot, "project-root", 0, required_argument, "Override project root for compile commands." },
    { RClient::RTagsConfig, "rtags-config", 0, required_argument, "Print out .rtags-config for argument." },
    { RClient::WildcardSymbolNames, "wildcard-symbol-names", 'a', no_argument, "Expand * like wildcards in --list-symbols and --find-symbols." },
    { RClient::NoColor, "no-color", 0, no_argument, "Don't colorize context. " },
    { RClient::None, 0, 0, 0, 0 }
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
    RCCommand() {}
    virtual ~RCCommand() {}
    virtual bool exec(RClient *rc, const std::shared_ptr<Connection> &connection) = 0;
    virtual String description() const = 0;
};

class QueryCommand : public RCCommand
{
public:
    QueryCommand(QueryMessage::Type t, const String &q)
        : RCCommand(), type(t), query(q)
    {}

    const QueryMessage::Type type;
    const String query;
    Flags<QueryMessage::Flag> extraQueryFlags;

    virtual bool exec(RClient *rc, const std::shared_ptr<Connection> &connection) override
    {
        QueryMessage msg(type);
        msg.init(rc->argc(), rc->argv());
        msg.setQuery(query);
        msg.setBuildIndex(rc->buildIndex());
        msg.setUnsavedFiles(rc->unsavedFiles());
        msg.setFlags(extraQueryFlags | rc->queryFlags());
        msg.setMax(rc->max());
        msg.setPathFilters(rc->pathFilters().toList());
        msg.setRangeFilter(rc->minOffset(), rc->maxOffset());
        msg.setCurrentFile(rc->currentFile());
        return connection->send(msg);
    }

    virtual String description() const override
    {
        return ("QueryMessage " + String::number(type) + " " + query);
    }
};

class QuitCommand : public RCCommand
{
public:
    QuitCommand(int exit)
        : RCCommand(), mExitCode(exit)
    {}

    virtual bool exec(RClient *, const std::shared_ptr<Connection> &connection) override
    {
        const QuitMessage msg(mExitCode);
        return connection->send(msg);
    }
    virtual String description() const override
    {
        return String::format<32>("QuitMessage(%d)", mExitCode);
    }
private:
    const int mExitCode;
};

class RdmLogCommand : public RCCommand
{
public:
    enum { Default = -3 };

    RdmLogCommand(int level)
        : RCCommand(), mLevel(level)
    {
    }
    virtual bool exec(RClient *rc, const std::shared_ptr<Connection> &connection) override
    {
        unsigned int flags = RTagsLogOutput::None;
        if (rc->queryFlags() & QueryMessage::ElispList)
            flags |= RTagsLogOutput::ElispList;
        const int level = mLevel == Default ? rc->logLevel() : mLevel;
        LogOutputMessage msg(level, flags);
        msg.init(rc->argc(), rc->argv());
        return connection->send(msg);
    }
    virtual String description() const override
    {
        return "RdmLogCommand";
    }
    const int mLevel;
};

class CompileCommand : public RCCommand
{
public:
    CompileCommand(const Path &c, const String &a, RClient::EscapeMode e)
        : RCCommand(), cwd(c), args(a), escapeMode(e)
    {}
    CompileCommand(const Path &dir, RClient::EscapeMode e)
        : RCCommand(), compilationDatabaseDir(dir), escapeMode(e)
    {}

    const Path compilationDatabaseDir;
    const Path cwd;
    const String args;
    const RClient::EscapeMode escapeMode;
    virtual bool exec(RClient *rc, const std::shared_ptr<Connection> &connection) override
    {
        bool escape = false;
        switch (rc->mEscapeMode) {
        case RClient::Escape_Auto:
            escape = (escapeMode == RClient::Escape_Do);
            break;
        case RClient::Escape_Do:
            escape = true;
            break;
        case RClient::Escape_Dont:
            escape = false;
            break;
        }
        IndexMessage msg;
        msg.init(rc->argc(), rc->argv());
        msg.setWorkingDirectory(cwd);
        msg.setFlag(IndexMessage::Escape, escape);
        msg.setFlag(IndexMessage::GuessFlags, rc->mGuessFlags);
        msg.setArguments(args);
        msg.setCompilationDatabaseDir(compilationDatabaseDir);
        if (!rc->projectRoot().isEmpty())
            msg.setProjectRoot(rc->projectRoot());

        return connection->send(msg);
    }
    virtual String description() const override
    {
        return ("IndexMessage " + cwd);
    }
};

RClient::RClient()
    : mMax(-1), mLogLevel(0), mTimeout(-1), mMinOffset(-1), mMaxOffset(-1),
      mConnectTimeout(DEFAULT_CONNECT_TIMEOUT), mBuildIndex(0),
      mEscapeMode(Escape_Auto), mGuessFlags(false), mArgc(0), mArgv(0)
{
}

RClient::~RClient()
{
    cleanupLogging();
}

void RClient::addQuery(QueryMessage::Type type, const String &query, Flags<QueryMessage::Flag> extraQueryFlags)
{
    std::shared_ptr<QueryCommand> cmd(new QueryCommand(type, query));
    cmd->extraQueryFlags = extraQueryFlags;
    mCommands.append(cmd);
}

void RClient::addQuitCommand(int exitCode)
{
    std::shared_ptr<QuitCommand> cmd(new QuitCommand(exitCode));
    mCommands.append(cmd);
}

void RClient::addLog(int level)
{
    mCommands.append(std::shared_ptr<RCCommand>(new RdmLogCommand(level)));
}

void RClient::addCompile(const Path &cwd, const String &args, EscapeMode mode)
{
    mCommands.append(std::shared_ptr<RCCommand>(new CompileCommand(cwd, args, mode)));
}

void RClient::addCompile(const Path &dir, EscapeMode mode)
{
    mCommands.append(std::shared_ptr<RCCommand>(new CompileCommand(dir, mode)));
}

int RClient::exec()
{
    RTags::initMessages();

    EventLoop::SharedPtr loop(new EventLoop);
    loop->init(EventLoop::MainEventLoop);

    const int commandCount = mCommands.size();
    std::shared_ptr<Connection> connection = Connection::create(NumOptions);
    connection->newMessage().connect(std::bind(&RClient::onNewMessage, this,
                                              std::placeholders::_1, std::placeholders::_2));
    connection->finished().connect(std::bind([](){ EventLoop::eventLoop()->quit(); }));
    connection->disconnected().connect(std::bind([](){ EventLoop::eventLoop()->quit(); }));
    if (!connection->connectUnix(mSocketFile, mConnectTimeout)) {
        if (mLogLevel >= Error)
            fprintf(stdout, "Can't seem to connect to server\n");
        return 1;
    }
    int ret = 0;
    for (int i=0; i<commandCount; ++i) {
        const std::shared_ptr<RCCommand> &cmd = mCommands.at(i);
        debug() << "running command " << cmd->description();
        if (!cmd->exec(this, connection) || loop->exec(timeout()) != EventLoop::Success) {
            ret = 1;
            break;
        }
    }
    if (connection->client())
        connection->client()->close();
    mCommands.clear();
    if (!ret)
        ret = connection->finishStatus();
    return ret;
}

RClient::ParseStatus RClient::parse(int &argc, char **argv)
{
    Rct::findExecutablePath(*argv);
    mSocketFile = Path::home() + ".rdm";

    List<option> options;
    options.reserve(sizeof(opts) / sizeof(Option));
    List<std::shared_ptr<QueryCommand> > projectCommands;

    String shortOptionString;
    Hash<int, Option*> shortOptions, longOptions;
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
        return Parse_Ok;
    }

    {
        const option opt = { 0, 0, 0, 0 };
        options.push_back(opt);
    }

    Path logFile;
    unsigned int logFlags = 0;

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

        if (!isatty(STDOUT_FILENO)) {
            mQueryFlags |= QueryMessage::NoColor;
        }

        switch (opt->option) {
        case None:
        case NumOptions:
            assert(0);
            break;
        case Help:
            help(stdout, argv[0]);
            return Parse_Ok;
        case Man:
            man();
            return Parse_Ok;
        case SocketFile:
            mSocketFile = optarg;
            break;
        case GuessFlags:
            mGuessFlags = true;
            break;
        case IMenu:
            mQueryFlags |= QueryMessage::IMenu;
            break;
        case CompilationFlagsOnly:
            mQueryFlags |= QueryMessage::CompilationFlagsOnly;
            break;
        case NoColor:
            mQueryFlags |= QueryMessage::NoColor;
            break;
        case CompilationFlagsSplitLine:
            mQueryFlags |= QueryMessage::CompilationFlagsSplitLine;
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
        case SymbolInfoIncludeParents:
            mQueryFlags |= QueryMessage::SymbolInfoIncludeParents;
            break;
        case SymbolInfoExcludeTargets:
            mQueryFlags |= QueryMessage::SymbolInfoExcludeTargets;
            break;
        case SymbolInfoExcludeReferences:
            mQueryFlags |= QueryMessage::SymbolInfoExcludeReferences;
            break;
        case CursorKind:
            mQueryFlags |= QueryMessage::CursorKind;
            break;
        case SynchronousCompletions:
            mQueryFlags |= QueryMessage::SynchronousCompletions;
            break;
        case DisplayName:
            mQueryFlags |= QueryMessage::DisplayName;
            break;
        case AllReferences:
            mQueryFlags |= QueryMessage::AllReferences;
            break;
        case AllTargets:
            mQueryFlags |= QueryMessage::AllTargets;
            break;
        case MatchCaseInsensitive:
            mQueryFlags |= QueryMessage::MatchCaseInsensitive;
            break;
        case MatchRegex:
            mQueryFlags |= QueryMessage::MatchRegex;
            break;
        case AbsolutePath:
            mQueryFlags |= QueryMessage::AbsolutePath;
            break;
        case ReverseSort:
            mQueryFlags |= QueryMessage::ReverseSort;
            break;
        case Rename:
            mQueryFlags |= QueryMessage::Rename;
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
        case PathFilter: {
            Path p = optarg;
            p.resolve();
            mPathFilters.insert(p);
            break; }
        case WildcardSymbolNames: {
            mQueryFlags |= QueryMessage::WildcardSymbolNames;
            break; }
        case RangeFilter: {
            std::cmatch caps;
            std::regex rx("^([0-9]+)-([0-9]+*)$");
            if (!Rct::contains(optarg, rx, &caps) || caps.size() != 3) {
                fprintf(stderr, "Can't parse range, must be uint-uint. E.g. 1-123\n");
                return Parse_Error;
            } else {
                mMinOffset = atoi(caps.str(1).c_str());
                mMaxOffset = atoi(caps.str(2).c_str());
                if (mMaxOffset <= mMinOffset || mMinOffset < 0) {
                    fprintf(stderr, "Invalid range (%d-%d), must be uint-uint. E.g. 1-123\n", mMinOffset, mMaxOffset);
                    return Parse_Error;
                }
            }
            break; }
        case Version:
            fprintf(stdout, "%s\n", RTags::versionString().constData());
            return Parse_Ok;
        case Verbose:
            ++mLogLevel;
            break;
        case PrepareCodeCompleteAt:
        case CodeCompleteAt: {
            std::cmatch caps;
            std::regex rx("^(.*):([0-9]+):([0-9]+):?");
            if (!Rct::contains(optarg, rx, &caps) || caps.size() != 4) {
                fprintf(stderr, "Can't decode argument for --code-complete-at [%s]\n", optarg);
                return Parse_Error;
            }
            const Path path = Path::resolved(caps.str(1), Path::MakeAbsolute);
            if (!path.isFile()) {
                fprintf(stderr, "Can't decode argument for --code-complete-at [%s]\n", optarg);
                return Parse_Error;
            }

            String out;
            {
                Serializer serializer(out);
                serializer << path << atoi(caps.str(2).c_str()) << atoi(caps.str(3).c_str());
            }
            addQuery(opt->option == CodeCompleteAt ? QueryMessage::CodeCompleteAt : QueryMessage::PrepareCodeCompleteAt, out);
            break; }
        case Silent:
            mLogLevel = -1;
            break;
        case LogFile:
            logFile = optarg;
            break;
        case StripParen:
            mQueryFlags |= QueryMessage::StripParentheses;
            break;
        case DumpIncludeHeaders:
            mQueryFlags |= QueryMessage::DumpIncludeHeaders;
            break;
        case SilentQuery:
            mQueryFlags |= QueryMessage::SilentQuery;
            break;
        case BuildIndex: {
            bool ok;
            mBuildIndex = String(optarg).toULongLong(&ok);
            if (!ok) {
                fprintf(stderr, "--build-index [arg] must be >= 0\n");
                return Parse_Error;
            }
            break; }
        case ConnectTimeout:
            mConnectTimeout = atoi(optarg);
            if (mConnectTimeout < 0) {
                fprintf(stderr, "--connect-timeout [arg] must be >= 0\n");
                return Parse_Error;
            }
            break;
        case Max:
            mMax = atoi(optarg);
            if (mMax <= 0) {
                fprintf(stderr, "-M [arg] must be positive integer\n");
                return Parse_Error;
            }
            break;
        case Timeout:
            mTimeout = atoi(optarg);
            if (mTimeout <= 0) {
                fprintf(stderr, "-y [arg] must be positive integer\n");
                return Parse_Error;
            }
            break;
        case UnsavedFile: {
            const String arg(optarg);
            const int colon = arg.lastIndexOf(':');
            if (colon == -1) {
                fprintf(stderr, "Can't parse -u [%s]\n", optarg);
                return Parse_Error;
            }
            const int bytes = atoi(arg.constData() + colon + 1);
            if (!bytes) {
                fprintf(stderr, "Can't parse -u [%s]\n", optarg);
                return Parse_Error;
            }
            const Path path = Path::resolved(arg.left(colon));
            if (!path.isFile()) {
                fprintf(stderr, "Can't open [%s] for reading\n", arg.left(colon).nullTerminated());
                return Parse_Error;
            }

            String contents(bytes, '\0');
            const int r = fread(contents.data(), 1, bytes, stdin);
            if (r != bytes) {
                fprintf(stderr, "Read error %d (%s). Got %d, expected %d\n",
                        errno, Rct::strerror(errno).constData(), r, bytes);
                return Parse_Error;
            }
            mUnsavedFiles[path] = contents;
            break; }
        case FollowLocation:
        case SymbolInfo:
        case ClassHierarchy:
        case ReferenceLocation: {
            const String encoded = Location::encode(optarg);
            if (encoded.isEmpty()) {
                fprintf(stderr, "Can't resolve argument %s\n", optarg);
                return Parse_Error;
            }
            QueryMessage::Type type = QueryMessage::Invalid;
            switch (opt->option) {
            case FollowLocation: type = QueryMessage::FollowLocation; break;
            case SymbolInfo: type = QueryMessage::SymbolInfo; break;
            case ReferenceLocation: type = QueryMessage::ReferencesLocation; break;
            case ClassHierarchy: type = QueryMessage::ClassHierarchy; break;
            default: assert(0); break;
            }
            addQuery(type, encoded, QueryMessage::HasLocation);
            break; }
        case CurrentFile:
            mCurrentFile.append(Path::resolved(optarg));
            break;
        case ReloadFileManager:
            addQuery(QueryMessage::ReloadFileManager);
            break;
        case DumpCompletions:
            addQuery(QueryMessage::DumpCompletions);
            break;
        case DumpCompilationDatabase:
            addQuery(QueryMessage::DumpCompilationDatabase);
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
        case QuitRdm: {
            const char *arg = 0;
            if (optarg) {
                arg = optarg;
            } else if (optind < argc && argv[optind][0] != '-') {
                arg = argv[optind++];
            }
            int exit = 0;
            if (arg) {
                bool ok;
                exit = String(arg).toLongLong(&ok);
                if (!ok) {
                    fprintf(stderr, "Invalid argument to -q\n");
                    return Parse_Error;
                }
            }
            addQuitCommand(exit);
            break; }
        case DeleteProject:
            addQuery(QueryMessage::DeleteProject, optarg);
            break;
        case SendDiagnostics:
            addQuery(QueryMessage::SendDiagnostics, optarg);
            break;
        case FindProjectRoot: {
            const Path p = Path::resolved(optarg);
            printf("findProjectRoot [%s] => [%s]\n", p.constData(),
                   RTags::findProjectRoot(p, RTags::SourceRoot).constData());
            return Parse_Ok; }
        case FindProjectBuildRoot: {
            const Path p = Path::resolved(optarg);
            printf("findProjectRoot [%s] => [%s]\n", p.constData(),
                   RTags::findProjectRoot(p, RTags::BuildRoot).constData());
            return Parse_Ok; }
        case RTagsConfig: {
            const Path p = Path::resolved(optarg);
            Map<String, String> config = RTags::rtagsConfig(p);
            printf("rtags-config: %s:\n", p.constData());
            for (const auto &it : config) {
                printf("%s: \"%s\"\n", it.first.constData(), it.second.constData());
            }
            return Parse_Ok; }
        case CheckReindex:
        case Reindex:
        case Project:
        case FindFile:
        case ListSymbols:
        case FindSymbols:
        case Sources:
        case IncludeFile:
        case JobCount:
        case Status: {
            Flags<QueryMessage::Flag> extraQueryFlags;
            QueryMessage::Type type = QueryMessage::Invalid;
            bool resolve = true;
            switch (opt->option) {
            case CheckReindex: type = QueryMessage::CheckReindex; break;
            case Reindex: type = QueryMessage::Reindex; break;
            case Project: type = QueryMessage::Project; break;
            case FindFile: type = QueryMessage::FindFile; resolve = false; break;
            case Sources: type = QueryMessage::Sources; break;
            case IncludeFile: type = QueryMessage::IncludeFile; resolve = false; break;
            case Status: type = QueryMessage::Status; break;
            case ListSymbols: type = QueryMessage::ListSymbols; break;
            case FindSymbols: type = QueryMessage::FindSymbols; break;
            case JobCount: type = QueryMessage::JobCount; break;
            default: assert(0); break;
            }

            const char *arg = 0;
            if (optarg) {
                arg = optarg;
            } else if (optind < argc && argv[optind][0] != '-') {
                arg = argv[optind++];
            }
            if (arg) {
                Path p(arg);
                if (resolve && p.exists()) {
                    p.resolve();
                    addQuery(type, p, extraQueryFlags);
                } else {
                    addQuery(type, arg, extraQueryFlags);
                }
            } else {
                addQuery(type, String(), extraQueryFlags);
            }
            assert(!mCommands.isEmpty());
            if (type == QueryMessage::Project)
                projectCommands.append(std::static_pointer_cast<QueryCommand>(mCommands.back()));
            break; }
        case ListBuffers:
            addQuery(QueryMessage::SetBuffers);
            break;
        case SetBuffers: {
            const char *arg = 0;
            if (optarg) {
                arg = optarg;
            } else if (optind < argc && (argv[optind][0] != '-' || !strcmp(argv[optind], "-"))) {
                arg = argv[optind++];
            }
            String encoded;
            if (arg) {
                List<Path> paths;
                auto addBuffer = [&paths](const String &p) {
                    if (p.isEmpty())
                        return;
                    Path path(p);
                    if (path.resolve() && path.isFile()) {
                        paths.append(path);
                    } else {
                        fprintf(stderr, "\"%s\" doesn't seem to be a file.\n", p.constData());
                    }
                };

                if (!strcmp(arg, "-")) {
                    char buf[1024];
                    while (fgets(buf, sizeof(buf), stdin)) {
                        String arg(buf);
                        if (arg.endsWith('\n'))
                            arg.chop(1);
                        addBuffer(arg);
                    }
                } else {
                    for (const String &buffer : String(arg).split(';')) {
                        addBuffer(buffer);
                    }
                }
                Serializer serializer(encoded);
                serializer << paths;
            }
            addQuery(QueryMessage::SetBuffers, encoded);
            break; }
        case LoadCompilationDatabase: {
#if CLANG_VERSION_MAJOR > 3 || (CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR > 3)
            Path dir;
            if (optarg) {
                dir = optarg;
            } else if (optind < argc && argv[optind][0] != '-') {
                dir = argv[optind++];
            } else {
                dir = Path::pwd();
            }
            dir.resolve(Path::MakeAbsolute);
            if (!dir.exists()) {
                fprintf(stderr, "%s does not seem to exist\n", dir.constData());
                return Parse_Error;
            }
            if (!dir.isDir()) {
                if (dir.isFile() && dir.endsWith("/compile_commands.json")) {
                    dir = dir.parentDir();
                } else {
                    fprintf(stderr, "%s is not a directory\n", dir.constData());
                    return Parse_Error;
                }
            }
            if (!dir.endsWith('/'))
                dir += '/';
            const Path file = dir + "compile_commands.json";
            if (!file.isFile()) {
                fprintf(stderr, "no compile_commands.json file in %s\n", dir.constData());
                return Parse_Error;
            }
            addCompile(dir, Escape_Auto);
#endif
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
                return Parse_Error;
            }
            if (p.isDir())
                p.append('/');
            addQuery(QueryMessage::HasFileManager, p);
            break; }
        case ProjectRoot: {
            Path p = optarg;
            if (!p.isDir()) {
                fprintf(stderr, "%s does not seem to be a directory\n", optarg);
                return Parse_Error;
            }

            p.resolve(Path::MakeAbsolute);
            mProjectRoot = p;
            break; }
        case Suspend: {
            Path p;
            if (optarg) {
                p = optarg;
            } else if (optind < argc && argv[optind][0] != '-') {
                p = argv[optind++];
            }
            if (!p.isEmpty()) {
                if (p != "clear" && p != "all") {
                    p.resolve(Path::MakeAbsolute);
                    if (!p.isFile()) {
                        fprintf(stderr, "%s is not a file\n", optarg);
                        return Parse_Error;
                    }
                }
            }
            addQuery(QueryMessage::Suspend, p);
            break; }
        case Compile: {
            String args = optarg;
            while (optind < argc) {
                if (!args.isEmpty())
                    args.append(' ');
                args.append(argv[optind++]);
            }
            if (args == "-" || args.isEmpty()) {
                char buf[16384];
                while (fgets(buf, sizeof(buf), stdin)) {
                    addCompile(Path::pwd(), buf, Escape_Do);
                }
            } else {
                addCompile(Path::pwd(), args, Escape_Dont);
            }
            break; }
        case IsIndexing:
            addQuery(QueryMessage::IsIndexing);
            break;
        case UnescapeCompileCommands:
            mEscapeMode = Escape_Do;
            break;
        case NoUnescapeCompileCommands:
            mEscapeMode = Escape_Dont;
            break;
        case NoSortReferencesByInput:
            mQueryFlags |= QueryMessage::NoSortReferencesByInput;
            break;
        case IsIndexed:
        case DumpFile:
        case Dependencies:
        case GenerateTest:
        case FixIts: {
            Path p = optarg;
            if (!p.exists()) {
                fprintf(stderr, "%s does not exist\n", optarg);
                return Parse_Error;
            }

            if (!p.isAbsolute())
                p.prepend(Path::pwd());

            if (p.isDir()) {
                if (opt->option != IsIndexed) {
                    fprintf(stderr, "%s is not a file\n", optarg);
                    return Parse_Error;
                } else if (!p.endsWith('/')) {
                    p.append('/');
                }
            }
            p.resolve();
            QueryMessage::Type type = QueryMessage::Invalid;
            switch (opt->option) {
            case GenerateTest: type = QueryMessage::GenerateTest; break;
            case Dependencies: type = QueryMessage::Dependencies; break;
            case FixIts: type = QueryMessage::FixIts; break;
            case DumpFile: type = QueryMessage::DumpFile; break;
            case IsIndexed: type = QueryMessage::IsIndexed; break;
            default: assert(0); break;
            }

            addQuery(type, p);
            break; }
        case PreprocessFile: {
            Path p = optarg;
            p.resolve(Path::MakeAbsolute);
            if (!p.isFile()) {
                fprintf(stderr, "%s is not a file\n", optarg);
                return Parse_Error;
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
        }
    }
    if (state == Error) {
        help(stderr, argv[0]);
        return Parse_Error;
    }

    if (optind < argc) {
        fprintf(stderr, "rc: unexpected option -- '%s'\n", argv[optind]);
        return Parse_Error;
    }

    if (!initLogging(argv[0], LogStderr, mLogLevel, logFile, logFlags)) {
        fprintf(stderr, "Can't initialize logging with %d %s 0x%0x\n",
                mLogLevel, logFile.constData(), logFlags);
        return Parse_Error;
    }


    if (mCommands.isEmpty()) {
        help(stderr, argv[0]);
        return Parse_Error;
    }
    if (mCommands.size() > projectCommands.size()) {
        // If there's more than one command one likely does not want output from
        // the queryCommand (unless there's no arg specified for it). This is so
        // we don't have to pass a different flag for auto-updating project
        // using the current buffer but rather piggy-back on --project
        const int count = projectCommands.size();
        for (int i=0; i<count; ++i) {
            std::shared_ptr<QueryCommand> &cmd = projectCommands[i];
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

    return Parse_Exec;
}

void RClient::onNewMessage(const std::shared_ptr<Message> &message, const std::shared_ptr<Connection> &)
{
    if (message->messageId() == ResponseMessage::MessageId) {
        const String response = std::static_pointer_cast<ResponseMessage>(message)->data();
        if (!response.isEmpty() && mLogLevel >= Error) {
            fprintf(stdout, "%s\n", response.constData());
            fflush(stdout);
        }
    } else {
        error("Unexpected message: %d", message->messageId());
    }
}
