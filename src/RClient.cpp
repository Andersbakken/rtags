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

#include <stdio.h>
#include <sys/ioctl.h>

#include "FileMap.h"
#include "IndexMessage.h"
#include "LogOutputMessage.h"
#include "rct/Connection.h"
#include "rct/EventLoop.h"
#include "rct/Log.h"
#include "rct/QuitMessage.h"
#include "rct/Rct.h"
#include "rct/StopWatch.h"
#include "rct/OnDestruction.h"
#include "RTags.h"
#include "RTagsLogOutput.h"

#define DEFAULT_CONNECT_TIMEOUT 1000
#define XSTR(s) #s
#define STR(s) XSTR(s)

struct CommandLineParser::Option<RClient::OptionType> opts[] = {
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
    { RClient::JobCount, "job-count", 'j', optional_argument, "Set or query current job count. (Prefix with l to set low-priority-job-count)." },

    { RClient::None, 0, 0, 0, "" },
    { RClient::None, 0, 0, 0, "Indexing commands:" },
    { RClient::Compile, "compile", 'c', optional_argument, "Pass compilation arguments to rdm." },
    { RClient::GuessFlags, "guess-flags", 0, no_argument, "Guess compile flags (used with -c)." },
    { RClient::LoadCompilationDatabase, "load-compilation-database", 'J', optional_argument, "Load compile_commands.json from directory" },
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
    { RClient::Diagnose, "diagnose", 0, required_argument, "Resend diagnostics for file." },
    { RClient::DiagnoseAll, "diagnose-all", 0, no_argument, "Resend diagnostics for all files." },
    { RClient::IsIndexed, "is-indexed", 'T', required_argument, "Check if rtags knows about, and is ready to return information about, this source file." },
    { RClient::IsIndexing, "is-indexing", 0, no_argument, "Check if rtags is currently indexing files." },
    { RClient::HasFileManager, "has-filemanager", 0, optional_argument, "Check if rtags has info about files in this directory." },
    { RClient::PreprocessFile, "preprocess", 'E', required_argument, "Preprocess file." },
    { RClient::Reindex, "reindex", 'V', optional_argument, "Reindex all files or all files matching pattern." },
    { RClient::CheckReindex, "check-reindex", 'x', optional_argument, "Check if reindexing is necessary for all files matching pattern." },
    { RClient::FindFile, "path", 'P', optional_argument, "Print files matching pattern." },
    { RClient::CurrentProject, "current-project", 0, no_argument, "Print path for current project." },
    { RClient::DumpFile, "dump-file", 'd', required_argument, "Dump source file." },
    { RClient::CheckIncludes, "check-includes", 0, required_argument, "Check includes for source file." },
    { RClient::DumpFileMaps, "dump-file-maps", 0, required_argument, "Dump file maps for file." },
    { RClient::GenerateTest, "generate-test", 0, required_argument, "Generate a test for a given source file." },
    { RClient::RdmLog, "rdm-log", 'g', no_argument, "Receive logs from rdm." },
    { RClient::FixIts, "fixits", 0, required_argument, "Get fixits for file." },
    { RClient::RemoveFile, "remove", 'D', required_argument, "Remove file from project." },
    { RClient::FindProjectRoot, "find-project-root", 0, required_argument, "Use to check behavior of find-project-root." },
    { RClient::FindProjectBuildRoot, "find-project-build-root", 0, required_argument, "Use to check behavior of find-project-root for builds." },
    { RClient::IncludeFile, "include-file", 0, required_argument, "Use to generate include statement for symbol." },
    { RClient::Sources, "sources", 0, optional_argument, "Dump sources for source file." },
    { RClient::Dependencies, "dependencies", 0, required_argument, "Dump dependencies for source file [(includes, included-by, depends-on, depended-on, tree-depends-on, raw)]." },
    { RClient::AllDependencies, "all-dependencies", 0, no_argument, "Dump dependencies for all source files [(includes, included-by, depends-on, depended-on, tree-depends-on, raw)]." },
    { RClient::ReloadFileManager, "reload-file-manager", 'B', no_argument, "Reload file manager." },
    { RClient::Man, "man", 0, no_argument, "Output XML for xmltoman to generate man page for rc :-)" },
    { RClient::CodeCompleteAt, "code-complete-at", 'l', required_argument, "Code complete at location: arg is file:line:col." },
    { RClient::SendDiagnostics, "send-diagnostics", 0, required_argument, "Only for debugging. Send data to all -G connections." },
    { RClient::DumpCompletions, "dump-completions", 0, no_argument, "Dump cached completions." },
    { RClient::DumpCompilationDatabase, "dump-compilation-database", 0, no_argument, "Dump compilation database for project." },
    { RClient::SetBuffers, "set-buffers", 0, optional_argument, "Set active buffers (list of filenames for active buffers in editor)." },
    { RClient::ListBuffers, "list-buffers", 0, no_argument, "List active buffers." },
    { RClient::ListCursorKinds, "list-cursor-kinds", 0, no_argument, "List spelling for known cursor kinds." },
    { RClient::ClassHierarchy, "class-hierarchy", 0, required_argument, "Dump class hierarcy for struct/class at location." },
    { RClient::DebugLocations, "debug-locations", 0, optional_argument, "Manipulate debug locations." },
#ifdef RTAGS_HAS_LUA
    { RClient::VisitAST, "visit-ast", 0, required_argument, "Visit AST of a source file." },
#endif
    { RClient::Tokens, "tokens", 0, required_argument, "Dump tokens for file. --tokens file.cpp:123-321 for range." },
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
    { RClient::DependencyFilter, "dependency-filter", 0, required_argument, "Filter out results unless argument depends on them." },
    { RClient::RangeFilter, "range-filter", 0, required_argument, "Filter out results not in the specified range." },
    { RClient::FilterSystemHeaders, "filter-system-headers", 'H', no_argument, "Don't exempt system headers from path filters." },
    { RClient::AllReferences, "all-references", 'e', no_argument, "Include definitions/declarations/constructors/destructors for references. Used for rename symbol." },
    { RClient::AllTargets, "all-targets", 0, no_argument, "Print all targets for -f. Used for debugging." },
    { RClient::Elisp, "elisp", 'Y', no_argument, "Output elisp: (list \"one\" \"two\" ...)." },
    { RClient::JSON, "json", 0, no_argument, "Output json." },
    { RClient::Diagnostics, "diagnostics", 'm', no_argument, "Receive async formatted diagnostics from rdm." },
    { RClient::MatchRegex, "match-regexp", 'Z', no_argument, "Treat various text patterns as regexps (-P, -i, -V)." },
    { RClient::MatchCaseInsensitive, "match-icase", 'I', no_argument, "Match case insensitively" },
    { RClient::AbsolutePath, "absolute-path", 'K', no_argument, "Print files with absolute path." },
    { RClient::SocketFile, "socket-file", 'n', required_argument, "Use this socket file (default ~/.rdm)." },
    { RClient::SocketAddress, "socket-address", 0, required_argument, "Use this host:port combination (instead of --socket-file)." },
    { RClient::Timeout, "timeout", 'y', required_argument, "Max time in ms to wait for job to finish (default no timeout)." },
    { RClient::FindVirtuals, "find-virtuals", 'k', no_argument, "Use in combinations with -R or -r to show other implementations of this function." },
    { RClient::FindFilePreferExact, "find-file-prefer-exact", 'A', no_argument, "Use to make --find-file prefer exact matches over partial matches." },
    { RClient::SymbolInfoIncludeParents, "symbol-info-include-parents", 0, no_argument, "Use to make --symbol-info include parent symbols." },
    { RClient::SymbolInfoIncludeTargets, "symbol-info-include-targets", 0, no_argument, "Use to make --symbol-info include target symbols." },
    { RClient::SymbolInfoIncludeReferences, "symbol-info-include-references", 0, no_argument, "Use to make --symbol-info include reference symbols." },
    { RClient::SymbolInfoIncludeBaseClasses, "symbol-info-include-base-classes", 0, no_argument, "Use to make --symbol-info include baseclasses' symbols." },
    { RClient::CursorKind, "cursor-kind", 0, no_argument, "Include cursor kind in --find-symbols output." },
    { RClient::DisplayName, "display-name", 0, no_argument, "Include display name in --find-symbols output." },
    { RClient::CurrentFile, "current-file", 0, required_argument, "Pass along which file is being edited to give rdm a better chance at picking the right project." },
    { RClient::DeclarationOnly, "declaration-only", 0, no_argument, "Filter out definitions (unless inline).", },
    { RClient::DefinitionOnly, "definition-only", 0, no_argument, "Filter out declarations (unless inline).", },
    { RClient::KindFilter, "kind-filter", 0, required_argument, "Only return results matching this kind.", },
    { RClient::ContainingFunction, "containing-function", 'o', no_argument, "Include name of containing function in output."},
    { RClient::ContainingFunctionLocation, "containing-function-location", 0, no_argument, "Include location of containing function in output."},
    { RClient::BuildIndex, "build-index", 0, required_argument, "For sources with multiple builds, use the arg'th." },
    { RClient::CompilationFlagsOnly, "compilation-flags-only", 0, no_argument, "For --source, only print compilation flags." },
    { RClient::CompilationFlagsSplitLine, "compilation-flags-split-line", 0, no_argument, "For --source, print one compilation flag per line." },
    { RClient::DumpIncludeHeaders, "dump-include-headers", 0, no_argument, "For --dump-file, also dump dependencies." },
    { RClient::SilentQuery, "silent-query", 0, no_argument, "Don't log this request in rdm." },
    { RClient::SynchronousCompletions, "synchronous-completions", 0, no_argument, "Wait for completion results and print them to stdout." },
    { RClient::SynchronousDiagnostics, "synchronous-diagnostics", 0, no_argument, "Wait for diagnostics and print them to stdout." },
    { RClient::XML, "xml", 0, no_argument, "Output XML" },
    { RClient::NoSortReferencesByInput, "no-sort-references-by-input", 0, no_argument, "Don't sort references by input position." },
    { RClient::ProjectRoot, "project-root", 0, required_argument, "Override project root for compile commands." },
    { RClient::RTagsConfig, "rtags-config", 0, required_argument, "Print out .rtags-config for argument." },
    { RClient::WildcardSymbolNames, "wildcard-symbol-names", 'a', no_argument, "Expand * like wildcards in --list-symbols and --find-symbols." },
    { RClient::NoColor, "no-color", 0, no_argument, "Don't colorize context." },
    { RClient::Wait, "wait", 0, no_argument, "Wait for reindexing to finish." },
    { RClient::Autotest, "autotest", 0, no_argument, "Turn on behaviors appropriate for running autotests." },
    { RClient::CodeCompleteIncludeMacros, "code-complete-include-macros", 0, no_argument, "Include macros in code completion results." },
    { RClient::CodeCompleteIncludes, "code-complete-includes", 0, no_argument, "Give includes in completion results." },
    { RClient::CodeCompleteNoWait, "code-complete-no-wait", 0, no_argument, "Don't wait for synchronous completion if the translation unit has to be created." },
    { RClient::CodeCompletionEnabled, "code-completion-enabled", 'b', no_argument, "Inform rdm that we're code-completing. Use with --diagnose" },
    { RClient::NoSpellCheckinging, "no-spell-checking", 0, no_argument, "Don't produce spell check info in diagnostics." },
#ifdef RTAGS_HAS_LUA
    { RClient::VisitASTScript, "visit-ast-script", 0, required_argument, "Use this script visit AST (@file.js|sourcecode)." },
#endif
    { RClient::TokensIncludeSymbols, "tokens-include-symbols", 0, no_argument, "Include symbols for tokens." },
    { RClient::None, 0, 0, 0, 0 }
};

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
        msg.setPathFilters(rc->pathFilters());
        msg.setKindFilters(rc->kindFilters());
        msg.setRangeFilter(rc->minOffset(), rc->maxOffset());
        msg.setTerminalWidth(rc->terminalWidth());
        msg.setCurrentFile(rc->currentFile());
#ifdef RTAGS_HAS_LUA
        msg.setVisitASTScripts(rc->visitASTScripts());
#endif
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
    static const LogLevel Default;

    RdmLogCommand(LogLevel level)
        : RCCommand(), mLevel(level)
    {
    }
    virtual bool exec(RClient *rc, const std::shared_ptr<Connection> &connection) override
    {
        unsigned int flags = RTagsLogOutput::None;
        if (rc->queryFlags() & QueryMessage::Elisp) {
            flags |= RTagsLogOutput::Elisp;
        } else if (rc->queryFlags() & QueryMessage::XML) {
            flags |= RTagsLogOutput::XML;
        } else if (rc->queryFlags() & QueryMessage::JSON) {
            flags |= RTagsLogOutput::JSON;
        } else if (rc->queryFlags() & QueryMessage::NoSpellChecking) {
            flags |= RTagsLogOutput::NoSpellChecking;
        }

        const LogLevel level = mLevel == Default ? rc->logLevel() : mLevel;
        LogOutputMessage msg(level, flags);
        msg.init(rc->argc(), rc->argv());
        return connection->send(msg);
    }
    virtual String description() const override
    {
        return "RdmLogCommand";
    }
    const LogLevel mLevel;
};

const LogLevel RdmLogCommand::Default(-1);

class CompileCommand : public RCCommand
{
public:
    CompileCommand(const Path &c, const String &a)
        : RCCommand(), cwd(c), args(a)
    {}
    CompileCommand(const Path &dir)
        : RCCommand(), compilationDatabaseDir(dir)
    {}

    const Path compilationDatabaseDir;
    const Path cwd;
    const String args;
    virtual bool exec(RClient *rc, const std::shared_ptr<Connection> &connection) override
    {
        IndexMessage msg;
        msg.init(rc->argc(), rc->argv());
        msg.setWorkingDirectory(cwd);
        msg.setFlag(IndexMessage::GuessFlags, rc->mGuessFlags);
        msg.setArguments(args);
        msg.setCompilationDatabaseDir(compilationDatabaseDir);
        msg.setEnvironment(rc->environment());
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
    : mMax(-1), mTimeout(-1), mMinOffset(-1), mMaxOffset(-1),
      mConnectTimeout(DEFAULT_CONNECT_TIMEOUT), mBuildIndex(0),
      mLogLevel(LogLevel::Error), mTcpPort(0), mGuessFlags(false),
      mTerminalWidth(-1), mArgc(0), mArgv(0)
{
    struct winsize w;
    ioctl(0, TIOCGWINSZ, &w);
    mTerminalWidth = w.ws_col;
    if (mTerminalWidth <= 0)
        mTerminalWidth = 1024;
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

void RClient::addLog(LogLevel level)
{
    mCommands.append(std::shared_ptr<RCCommand>(new RdmLogCommand(level)));
}

void RClient::addCompile(const Path &cwd, const String &args)
{
    mCommands.append(std::shared_ptr<RCCommand>(new CompileCommand(cwd, args)));
}

void RClient::addCompile(const Path &dir)
{
    mCommands.append(std::shared_ptr<RCCommand>(new CompileCommand(dir)));
}

int RClient::exec()
{
    RTags::initMessages();
    OnDestruction onDestruction([]() { Message::cleanup(); });
    EventLoop::SharedPtr loop(new EventLoop);
    loop->init(EventLoop::MainEventLoop);

    const int commandCount = mCommands.size();
    std::shared_ptr<Connection> connection = Connection::create(NumOptions);
    connection->newMessage().connect(std::bind(&RClient::onNewMessage, this,
                                               std::placeholders::_1, std::placeholders::_2));
    connection->finished().connect(std::bind([](){ EventLoop::eventLoop()->quit(); }));
    connection->disconnected().connect(std::bind([](){ EventLoop::eventLoop()->quit(); }));
    if (mTcpPort) {
        if (!connection->connectTcp(mTcpHost, mTcpPort, mConnectTimeout)) {
            if (mLogLevel >= LogLevel::Error)
                fprintf(stdout, "Can't seem to connect to server (%s:%d)\n", mTcpHost.constData(), mTcpPort);
            return 1;
        }
        connection->connected().connect(std::bind(&EventLoop::quit, loop.get()));
        loop->exec(mConnectTimeout);
        if (!connection->isConnected()) {
            if (mLogLevel >= LogLevel::Error) {
                if (mTcpPort) {
                    fprintf(stdout, "Can't seem to connect to server (%s:%d)\n", mTcpHost.constData(), mTcpPort);
                } else {
                    fprintf(stdout, "Can't seem to connect to server (%s)\n", mSocketFile.constData());
                }
            }
            return 1;
        }
    } else if (!connection->connectUnix(mSocketFile, mConnectTimeout)) {
        if (mLogLevel >= LogLevel::Error)
            fprintf(stdout, "Can't seem to connect to server (%s)\n", mSocketFile.constData());
        return 1;
    }

    int ret = 0;
    bool hasZeroExit = false;
    for (int i=0; i<commandCount; ++i) {
        const std::shared_ptr<RCCommand> &cmd = mCommands.at(i);
        debug() << "running command " << cmd->description();
        if (!cmd->exec(this, connection) || loop->exec(timeout()) != EventLoop::Success) {
            ret = 1;
            break;
        }
        if (connection->finishStatus() == 0)
            hasZeroExit = true;
    }
    if (connection->client())
        connection->client()->close();
    mCommands.clear();
    if (!ret && !(mFlags & Flag_Autotest) && !hasZeroExit)
        ret = connection->finishStatus();
    return ret;
}

CommandLineParser::ParseStatus RClient::parse(int &argc, char **argv)
{
    Rct::findExecutablePath(*argv);
    mSocketFile = Path::home() + ".rdm";

    List<std::shared_ptr<QueryCommand> > projectCommands;

    Path logFile;
    Flags<LogFlag> logFlags = LogStderr;

    if (!isatty(STDOUT_FILENO)) {
        mQueryFlags |= QueryMessage::NoColor;
    }

    std::function<CommandLineParser::ParseStatus(RClient::OptionType type)> cb;
    cb = [&](RClient::OptionType type) -> CommandLineParser::ParseStatus {
        switch (type) {
        case None:
        case NumOptions:
            assert(0);
            break;
        case Help:
            CommandLineParser::help(stdout, argv[0], opts, sizeof(opts) / sizeof(opts[0]));
            return CommandLineParser::Parse_Ok;
        case Man:
            CommandLineParser::man(opts, sizeof(opts) / sizeof(opts[0]));
            return CommandLineParser::Parse_Ok;
        case SocketFile:
            mSocketFile = optarg;
            break;
        case SocketAddress: {
            mTcpHost.assign(optarg);
            const int colon = mTcpHost.lastIndexOf(':');
            if (colon == -1) {
                fprintf(stderr, "invalid --socket-address %s\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            mTcpPort = atoi(optarg + colon + 1);
            if (!mTcpPort) {
                fprintf(stderr, "invalid --socket-address %s\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            mTcpHost.truncate(colon);
            break;
        }
        case GuessFlags:
            mGuessFlags = true;
            break;
        case Wait:
            mQueryFlags |= QueryMessage::Wait;
            break;
        case NoSpellCheckinging:
            mQueryFlags |= QueryMessage::NoSpellChecking;
            break;
        case CodeCompleteIncludeMacros:
            mQueryFlags |= QueryMessage::CodeCompleteIncludeMacros;
            break;
        case CodeCompleteIncludes:
            mQueryFlags |= QueryMessage::CodeCompleteIncludes;
            break;
        case CodeCompleteNoWait:
            mQueryFlags |= QueryMessage::CodeCompleteNoWait;
            break;
        case CodeCompletionEnabled:
            mQueryFlags |= QueryMessage::CodeCompletionEnabled;
            break;
        case Autotest:
            mFlags |= Flag_Autotest;
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
        case ContainingFunctionLocation:
            mQueryFlags |= QueryMessage::ContainingFunctionLocation;
            break;
        case DeclarationOnly:
            mQueryFlags |= QueryMessage::DeclarationOnly;
            break;
        case DefinitionOnly:
            mQueryFlags |= QueryMessage::DefinitionOnly;
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
        case SymbolInfoIncludeTargets:
            mQueryFlags |= QueryMessage::SymbolInfoIncludeTargets;
            break;
        case SymbolInfoIncludeReferences:
            mQueryFlags |= QueryMessage::SymbolInfoIncludeReferences;
            break;
        case SymbolInfoIncludeBaseClasses:
            mQueryFlags |= QueryMessage::SymbolInfoIncludeBaseClasses;
            break;
        case CursorKind:
            mQueryFlags |= QueryMessage::CursorKind;
            break;
        case SynchronousCompletions:
            mQueryFlags |= QueryMessage::SynchronousCompletions;
            break;
        case SynchronousDiagnostics:
            mQueryFlags |= QueryMessage::SynchronousDiagnostics;
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
        case Elisp:
            mQueryFlags |= QueryMessage::Elisp;
            break;
        case JSON:
            mQueryFlags |= QueryMessage::JSON;
            break;
        case XML:
            mQueryFlags |= QueryMessage::XML;
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
            mPathFilters.insert({ p, QueryMessage::PathFilter::Self });
            break;
        }
        case DependencyFilter: {
            Path p = optarg;
            p.resolve();
            if (!p.isFile()) {
                fprintf(stderr, "%s doesn't seem to be a file\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            mPathFilters.insert({ p, QueryMessage::PathFilter::Dependency });
            break;
        }
        case KindFilter:
            mKindFilters.insert(optarg);
            break;
        case WildcardSymbolNames:
            mQueryFlags |= QueryMessage::WildcardSymbolNames;
            break;
        case RangeFilter: {
            char *end;
            mMinOffset = strtoul(optarg, &end, 10);
            if (*end != '-') {
                fprintf(stderr, "Can't parse range, must be uint-uint. E.g. 1-123\n");
                return CommandLineParser::Parse_Error;
            }
            mMaxOffset = strtoul(end + 1, &end, 10);
            if (*end) {
                fprintf(stderr, "Can't parse range, must be uint-uint. E.g. 1-123\n");
                return CommandLineParser::Parse_Error;
            }
            if (mMaxOffset <= mMinOffset || mMinOffset < 0) {
                fprintf(stderr, "Invalid range (%d-%d), must be uint-uint. E.g. 1-123\n", mMinOffset, mMaxOffset);
                return CommandLineParser::Parse_Error;
            }
            break;
        }
        case Version:
            fprintf(stdout, "%s\n", RTags::versionString().constData());
            return CommandLineParser::Parse_Ok;
        case Verbose:
            ++mLogLevel;
            break;
        case CodeCompleteAt: {
            const String encoded = Location::encode(optarg);
            if (encoded.isEmpty()) {
                fprintf(stderr, "Can't resolve argument %s\n", optarg);
                return CommandLineParser::Parse_Error;
            }

            addQuery(QueryMessage::CodeCompleteAt, encoded);
            break;
        }
        case Silent:
            mLogLevel = LogLevel::None;
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
                return CommandLineParser::Parse_Error;
            }
            break;
        }
        case ConnectTimeout:
            mConnectTimeout = atoi(optarg);
            if (mConnectTimeout < 0) {
                fprintf(stderr, "--connect-timeout [arg] must be >= 0\n");
                return CommandLineParser::Parse_Error;
            }
            break;
        case Max:
            mMax = atoi(optarg);
            if (mMax < 0) {
                fprintf(stderr, "-M [arg] must be >= 0\n");
                return CommandLineParser::Parse_Error;
            }
            break;
        case Timeout:
            mTimeout = atoi(optarg);
            if (!mTimeout) {
                mTimeout = -1;
            } else if (mTimeout < 0) {
                fprintf(stderr, "-y [arg] must be >= 0\n");
                return CommandLineParser::Parse_Error;
            }
            break;
        case UnsavedFile: {
            const String arg(optarg);
            const int colon = arg.lastIndexOf(':');
            if (colon == -1) {
                fprintf(stderr, "Can't parse -u [%s]\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            const int bytes = atoi(arg.constData() + colon + 1);
            if (!bytes) {
                fprintf(stderr, "Can't parse -u [%s]\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            const Path path = Path::resolved(arg.left(colon));
            if (!path.isFile()) {
                fprintf(stderr, "Can't open [%s] for reading\n", arg.left(colon).nullTerminated());
                return CommandLineParser::Parse_Error;
            }

            String contents(bytes, '\0');
            const int r = fread(contents.data(), 1, bytes, stdin);
            if (r != bytes) {
                fprintf(stderr, "Read error %d (%s). Got %d, expected %d\n", errno, Rct::strerror(errno).constData(), r, bytes);
                return CommandLineParser::Parse_Error;
            }
            mUnsavedFiles[path] = contents;
            break;
        }
        case FollowLocation:
        case ClassHierarchy:
        case ReferenceLocation: {
            const String encoded = Location::encode(optarg);
            if (encoded.isEmpty()) {
                fprintf(stderr, "Can't resolve argument %s\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            QueryMessage::Type queryType = QueryMessage::Invalid;
            switch (type) {
            case FollowLocation:
                queryType = QueryMessage::FollowLocation;
                break;
            case ReferenceLocation:
                queryType = QueryMessage::ReferencesLocation;
                break;
            case ClassHierarchy:
                queryType = QueryMessage::ClassHierarchy;
                break;
            default:
                assert(0);
                break;
            }
            addQuery(queryType, encoded, QueryMessage::HasLocation);
            break; }
        case SymbolInfo: {
            std::cmatch match;
            std::regex rx("^(.*):([0-9]+):([0-9]+):?-:?([0-9]+):([0-9]+):?");
            Path path;
            uint32_t line = 0, col = 0, line2 = 0, col2 = 0;
            if (std::regex_match(optarg, match, rx)) {
                path.assign(optarg, match.length(1));
                line = atoi(optarg + match.position(2));
                col = atoi(optarg + match.position(3));
                line2 = atoi(optarg + match.position(4));
                col2 = atoi(optarg + match.position(5));
                if (!line || !col || !line2 || !col2 || !path.resolve(Path::MakeAbsolute)) {
                    fprintf(stderr, "Can't parse range %s\n", optarg);
                    return CommandLineParser::Parse_Error;
                }
            } else if (!Location::parse(optarg, Path(), Path::MakeAbsolute, &path, &line, &col)) {
                fprintf(stderr, "Can't parse range %s\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            String query;
            Serializer serializer(query);
            serializer << path << line << col << line2 << col2;
            addQuery(QueryMessage::SymbolInfo, query);
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
        case Clear:
            addQuery(QueryMessage::ClearProjects);
            break;
        case RdmLog:
            addLog(RdmLogCommand::Default);
            break;
        case Diagnostics:
            addLog(RTags::DiagnosticsLevel);
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
                    return CommandLineParser::Parse_Error;
                }
            }
            addQuitCommand(exit);
            break;
        }
        case DeleteProject:
            addQuery(QueryMessage::DeleteProject, optarg);
            break;
        case DebugLocations: {
            String arg;
            if (optarg) {
                arg = optarg;
            } else if (optind < argc && argv[optind][0] != '-') {
                arg = argv[optind++];
            }
            addQuery(QueryMessage::DebugLocations, arg);
            break;
        }
        case SendDiagnostics:
            addQuery(QueryMessage::SendDiagnostics, optarg);
            break;
        case FindProjectRoot: {
            const Path p = Path::resolved(optarg);
            printf("findProjectRoot [%s] => [%s]\n", p.constData(), RTags::findProjectRoot(p, RTags::SourceRoot).constData());
            return CommandLineParser::Parse_Ok;
        }
        case FindProjectBuildRoot: {
            const Path p = Path::resolved(optarg);
            printf("findProjectRoot [%s] => [%s]\n", p.constData(), RTags::findProjectRoot(p, RTags::BuildRoot).constData());
            return CommandLineParser::Parse_Ok;
        }
        case RTagsConfig: {
            const Path p = Path::resolved(optarg);
            Map<String, String> config = RTags::rtagsConfig(p);
            printf("rtags-config: %s:\n", p.constData());
            for (const auto &it : config) {
                printf("%s: \"%s\"\n", it.first.constData(), it.second.constData());
            }
            return CommandLineParser::Parse_Ok;
        }
        case CurrentProject:
            addQuery(QueryMessage::Project, String(), QueryMessage::CurrentProjectOnly);
            break;
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
            QueryMessage::Type queryType = QueryMessage::Invalid;
            bool resolve = true;
            switch (type) {
            case CheckReindex:
                queryType = QueryMessage::CheckReindex;
                break;
            case Reindex:
                queryType = QueryMessage::Reindex;
                break;
            case Project:
                queryType = QueryMessage::Project;
                break;
            case FindFile:
                queryType = QueryMessage::FindFile;
                resolve = false;
                break;
            case Sources:
                queryType = QueryMessage::Sources;
                break;
            case IncludeFile:
                queryType = QueryMessage::IncludeFile;
                resolve = false;
                break;
            case Status:
                queryType = QueryMessage::Status;
                break;
            case ListSymbols:
                queryType = QueryMessage::ListSymbols;
                break;
            case FindSymbols:
                queryType = QueryMessage::FindSymbols;
                break;
            case JobCount:
                queryType = QueryMessage::JobCount;
                break;
            default:
                assert(0);
                break;
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
                    addQuery(queryType, p, extraQueryFlags);
                } else {
                    addQuery(queryType, arg, extraQueryFlags);
                }
            } else {
                addQuery(queryType, String(), extraQueryFlags);
            }
            assert(!mCommands.isEmpty());
            if (queryType == QueryMessage::Project)
                projectCommands.append(std::static_pointer_cast<QueryCommand>(mCommands.back()));
            break; }
        case ListBuffers:
            addQuery(QueryMessage::SetBuffers);
            break;
        case ListCursorKinds: {
            auto print = [](CXCursorKind from, CXCursorKind to) {
                for (int i = from; i <= to; ++i) {
                    const CXCursorKind kind = static_cast<CXCursorKind>(i);
                    String spelling = RTags::eatString(clang_getCursorKindSpelling(kind));
                    spelling.remove(' ');
                    Log(LogLevel::Error, LogOutput::StdOut | LogOutput::TrailingNewLine) << spelling;
                }
            };
            Log(LogLevel::Error, LogOutput::StdOut | LogOutput::TrailingNewLine) << "Declarations:";
            print(CXCursor_FirstDecl, CXCursor_LastDecl);
            print(CXCursor_FirstExtraDecl, CXCursor_LastExtraDecl);
            Log(LogLevel::Error, LogOutput::StdOut | LogOutput::TrailingNewLine) << "References:";
            print(CXCursor_FirstRef, CXCursor_LastRef);
            Log(LogLevel::Error, LogOutput::StdOut | LogOutput::TrailingNewLine) << "Expressions:";
            print(CXCursor_FirstExpr, CXCursor_LastExpr);
            Log(LogLevel::Error, LogOutput::StdOut | LogOutput::TrailingNewLine) << "Statements:";
            print(CXCursor_FirstStmt, CXCursor_LastStmt);
            Log(LogLevel::Error, LogOutput::StdOut | LogOutput::TrailingNewLine) << "Attributes:";
            print(CXCursor_FirstAttr, CXCursor_LastAttr);
            Log(LogLevel::Error, LogOutput::StdOut | LogOutput::TrailingNewLine) << "Preprocessing:";
            print(CXCursor_FirstPreprocessing, CXCursor_LastPreprocessing);
            return CommandLineParser::Parse_Ok; }
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
                return CommandLineParser::Parse_Error;
            }
            if (!dir.isDir()) {
                if (dir.isFile() && dir.endsWith("/compile_commands.json")) {
                    dir = dir.parentDir();
                } else {
                    fprintf(stderr, "%s is not a directory\n", dir.constData());
                    return CommandLineParser::Parse_Error;
                }
            }
            if (!dir.endsWith('/'))
                dir += '/';
            const Path file = dir + "compile_commands.json";
            if (!file.isFile()) {
                fprintf(stderr, "no compile_commands.json file in %s\n", dir.constData());
                return CommandLineParser::Parse_Error;
            }
            addCompile(dir);
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
                return CommandLineParser::Parse_Error;
            }
            if (p.isDir() && !p.endsWith('/'))
                p.append('/');
            addQuery(QueryMessage::HasFileManager, p);
            break; }
        case ProjectRoot: {
            Path p = optarg;
            if (!p.isDir()) {
                fprintf(stderr, "%s does not seem to be a directory\n", optarg);
                return CommandLineParser::Parse_Error;
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
                        return CommandLineParser::Parse_Error;
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
                String pending;
                char buf[16384];
                while (fgets(buf, sizeof(buf), stdin)) {
                    pending += buf;
                    if (!pending.endsWith("\\\n")) {
                        addCompile(Path::pwd(), pending);
                        pending.clear();
                    } else {
                        memset(pending.data() + pending.size() - 2, ' ', 2);
                    }
                }
                if (!pending.isEmpty()) {
                    addCompile(Path::pwd(), pending);
                }
            } else {
                addCompile(Path::pwd(), args);
            }
            break; }
        case IsIndexing:
            addQuery(QueryMessage::IsIndexing);
            break;
        case NoSortReferencesByInput:
            mQueryFlags |= QueryMessage::NoSortReferencesByInput;
            break;
        case DiagnoseAll:
            addQuery(QueryMessage::Diagnose, String());
            break;
        case IsIndexed:
        case DumpFile:
        case CheckIncludes:
        case GenerateTest:
        case Diagnose:
        case FixIts: {
            Path p = optarg;
            if (!p.exists()) {
                fprintf(stderr, "%s does not exist\n", optarg);
                return CommandLineParser::Parse_Error;
            }

            if (!p.isAbsolute())
                p.prepend(Path::pwd());

            if (p.isDir()) {
                if (type != IsIndexed) {
                    fprintf(stderr, "%s is not a file\n", optarg);
                    return CommandLineParser::Parse_Error;
                } else if (!p.endsWith('/')) {
                    p.append('/');
                }
            }
            p.resolve();
            Flags<QueryMessage::Flag> extraQueryFlags;
            QueryMessage::Type queryType = QueryMessage::Invalid;
            switch (type) {
            case GenerateTest:
                queryType = QueryMessage::GenerateTest;
                break;
            case FixIts:
                queryType = QueryMessage::FixIts;
                break;
            case DumpFile:
                queryType = QueryMessage::DumpFile;
                break;
            case CheckIncludes:
                queryType = QueryMessage::DumpFile;
                extraQueryFlags |= QueryMessage::DumpCheckIncludes;
                break;
            case Diagnose:
                queryType = QueryMessage::Diagnose;
                break;
            case IsIndexed:
                queryType = QueryMessage::IsIndexed;
                break;
            default:
                assert(0);
                break;
            }

            addQuery(queryType, p, extraQueryFlags);
            break; }
        case AllDependencies: {
            String encoded;
            List<String> args;
            while (optind < argc && argv[optind][0] != '-') {
                args.append(argv[optind++]);
            }
            Serializer s(encoded);
            s << Path() << args;
            addQuery(QueryMessage::Dependencies, encoded);
            break; }
        case DumpFileMaps:
        case Dependencies: {
            Path p = optarg;
            if (!p.isFile()) {
                fprintf(stderr, "%s is not a file\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            p.resolve();
            List<String> args;
            while (optind < argc && argv[optind][0] != '-') {
                args.append(argv[optind++]);
            }

            String encoded;
            Serializer s(encoded);
            s << p << args;
            addQuery(type == DumpFileMaps ? QueryMessage::DumpFileMaps : QueryMessage::Dependencies, encoded);
            break; }
        case Tokens: {
            char path[PATH_MAX];
            uint32_t from, to;
            if (sscanf(optarg, "%[^':']:%u-%u", path, &from, &to) != 3) {
                if (sscanf(optarg, "%[^':']:%u-", path, &from) == 2) {
                    to = UINT_MAX;
                } else if (sscanf(optarg, "%[^':']:-%u", path, &to) == 2) {
                    from = 0;
                } else {
                    strncpy(path, optarg, strlen(optarg));
                    from = 0;
                    to = UINT_MAX;
                }
            }

            const Path p = Path::resolved(path);
            if (!p.isFile()) {
                fprintf(stderr, "%s is not a file\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            if (from >= to) {
                fprintf(stderr, "Invalid range: %s\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            String data;
            Serializer s(data);
            s << p << from << to;
            addQuery(QueryMessage::Tokens, data);
            break; }
        case TokensIncludeSymbols:
            mQueryFlags |= QueryMessage::TokensIncludeSymbols;
            break;
        case PreprocessFile: {
            Path p = optarg;
            p.resolve(Path::MakeAbsolute);
            if (!p.isFile()) {
                fprintf(stderr, "%s is not a file\n", optarg);
                return CommandLineParser::Parse_Error;
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
#ifdef RTAGS_HAS_LUA
        case VisitAST: {
            Path p = optarg;
            p.resolve(Path::MakeAbsolute);
            if (!p.isFile()) {
                fprintf(stderr, "%s is not a file\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            addQuery(QueryMessage::VisitAST, p);
            break; }
        case VisitASTScript: {
            String code = optarg;
            if (code.startsWith("@")) {
                const Path p = code.mid(1);
                if (!p.isFile()) {
                    fprintf(stderr, "%s is not a file\n", p.constData());
                    return CommandLineParser::Parse_Error;
                }
                code = p.readAll();
            }
            if (code.isEmpty()) {
                fprintf(stderr, "Script is empty\n");
                return CommandLineParser::Parse_Error;
            }
            mVisitASTScripts.push_back(code);
            break; }
#endif
        }
        return CommandLineParser::Parse_Exec;
    };

    const auto ret = CommandLineParser::parse<RClient::OptionType>(argc, argv, opts, sizeof(opts) / sizeof(opts[0]), NullFlags, cb);
    switch (ret) {
    case CommandLineParser::Parse_Error:
        fprintf(stderr, "Try 'rc --help' for more information.\n");
        // fall through
    case CommandLineParser::Parse_Ok:
        return ret;
    case CommandLineParser::Parse_Exec:
        break;
    }

    if (!initLogging(argv[0], logFlags, mLogLevel, logFile)) {
        fprintf(stderr, "Can't initialize logging with %d %s %s\n", mLogLevel.toInt(), logFile.constData(),
                logFlags.toString().constData());
        return CommandLineParser::Parse_Error;
    }

    if (mCommands.isEmpty()) {
        help(stderr, argv[0], opts, sizeof(opts) / sizeof(opts[0]));
        return CommandLineParser::Parse_Error;
    }
    if (mCommands.size() > projectCommands.size()) {
        // If there's more than one command one likely does not want output from
        // the queryCommand (unless there's no arg specified for it). This is so
        // we don't have to pass a different flag for auto-updating project
        // using the current buffer but rather piggy-back on --project
        const int count = projectCommands.size();
        for (int i = 0; i < count; ++i) {
            std::shared_ptr<QueryCommand> &cmd = projectCommands[i];
            if (!cmd->query.isEmpty()) {
                cmd->extraQueryFlags |= QueryMessage::Silent;
            }
        }
    }

    if (!logFile.isEmpty() || mLogLevel > LogLevel::Error) {
        Log l(LogLevel::Warning);
        l << argc;
        for (int i = 0; i < argc; ++i)
            l << " " << argv[i];
    }
    mArgc = argc;
    mArgv = argv;

    return CommandLineParser::Parse_Exec;
}

void RClient::onNewMessage(const std::shared_ptr<Message> &message, const std::shared_ptr<Connection> &)
{
    if (message->messageId() == ResponseMessage::MessageId) {
        const String response = std::static_pointer_cast<ResponseMessage>(message)->data();
        if (!response.isEmpty() && mLogLevel >= LogLevel::Error) {
            fprintf(stdout, "%s\n", response.constData());
            fflush(stdout);
        }
    } else {
        error("Unexpected message: %d", message->messageId());
    }
}

List<String> RClient::environment() const
{
    if (mEnvironment.isEmpty()) {
        mEnvironment = Rct::environment();
    }
    return mEnvironment;
}
