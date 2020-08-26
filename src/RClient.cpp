/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#include "RClient.h"

#include <stdio.h>
#include <sys/ioctl.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <functional>
#include <initializer_list>
#include <regex>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

#include "IndexMessage.h"
#include "LogOutputMessage.h"
#include "rct/Connection.h"
#include "rct/EventLoop.h"
#include "rct/Log.h"
#include "rct/QuitMessage.h"
#include "rct/Rct.h"
#include "rct/OnDestruction.h"
#include "RTags.h"
#include "Location.h"
#include "clang-c/Index.h"
#include "rct/Hash.h"
#include "rct/Map.h"
#include "rct/Message.h"
#include "rct/ResponseMessage.h"
#include "rct/Serializer.h"
#include "rct/SignalSlot.h"
#include "rct/SocketClient.h"

#define DEFAULT_CONNECT_TIMEOUT 1000
#define XSTR(s) #s
#define STR(s) XSTR(s)

std::initializer_list<CommandLineParser::Option<RClient::OptionType> > opts = {
    { RClient::None, String(), 0, CommandLineParser::NoValue, "Options:" },
    { RClient::Verbose, "verbose", 'v', CommandLineParser::NoValue, "Be more verbose." },
    { RClient::Version, "version", 0, CommandLineParser::NoValue, "Print current version." },
    { RClient::VerifyVersion, "verify-version", 't', CommandLineParser::Required, "Verify that the correct protocol version is used." },
    { RClient::Silent, "silent", 'Q', CommandLineParser::NoValue, "Be silent." },
    { RClient::Help, "help", 'h', CommandLineParser::NoValue, "Display this help." },
    { RClient::Noop, "config", 0, CommandLineParser::Required, "Use this file (instead of ~/.rcrc)." },
    { RClient::Noop, "no-rc", 0, CommandLineParser::NoValue, "Don't load any rc files." },

    { RClient::None, String(), 0, CommandLineParser::NoValue, "" },
    { RClient::None, String(), 0, CommandLineParser::NoValue, "Rdm:" },
    { RClient::QuitRdm, "quit-rdm", 'q', CommandLineParser::NoValue, "Tell server to shut down with optional exit code as argument." },
    { RClient::ConnectTimeout, "connect-timeout", 0, CommandLineParser::Required, "Timeout for connecting to rdm in ms (default " STR(DEFAULT_CONNECT_TIMEOUT)  ")." },

    { RClient::None, String(), 0, CommandLineParser::NoValue, "" },
    { RClient::None, String(), 0, CommandLineParser::NoValue, "Project management:" },
    { RClient::Clear, "clear", 'C', CommandLineParser::NoValue, "Clear projects." },
    { RClient::Project, "project", 'w', CommandLineParser::Optional, "With arg, select project matching that if unique, otherwise list all projects." },
    { RClient::DeleteProject, "delete-project", 'W', CommandLineParser::Required, "Delete all projects matching regex." },
    { RClient::JobCount, "job-count", 'j', CommandLineParser::Optional, "Set or query current job count. (Prefix with l to set low-priority-job-count)." },

    { RClient::None, String(), 0, CommandLineParser::NoValue, "" },
    { RClient::None, String(), 0, CommandLineParser::NoValue, "Indexing commands:" },
    { RClient::Compile, "compile", 'c', CommandLineParser::Optional, "Pass compilation arguments to rdm." },
    { RClient::GuessFlags, "guess-flags", 0, CommandLineParser::NoValue, "Guess compile flags (used with -c)." },
    { RClient::LoadCompileCommands, "load-compile-commands", 'J', CommandLineParser::Optional, "Load compile_commands.json from directory" },
    { RClient::Suspend, "suspend", 'X', CommandLineParser::Optional, "Dump suspended files (don't track changes in these files) with no arg. Otherwise toggle suspension for arg." },

    { RClient::None, String(), 0, CommandLineParser::NoValue, "" },
    { RClient::None, String(), 0, CommandLineParser::NoValue, "Query commands:" },
    { RClient::FollowLocation, "follow-location", 'f', CommandLineParser::Required, "Follow this location." },
    { RClient::ReferenceName, "references-name", 'R', CommandLineParser::Required, "Find references matching arg." },
    { RClient::ReferenceLocation, "references", 'r', CommandLineParser::Required, "Find references matching this location." },
    { RClient::ListSymbols, "list-symbols", 'S', CommandLineParser::Optional, "List symbol names matching arg." },
    { RClient::FindSymbols, "find-symbols", 'F', CommandLineParser::Optional, "Find symbols matching arg." },
    { RClient::SymbolInfo, "symbol-info", 'U', CommandLineParser::Required, "Get cursor info for this location." },
    { RClient::Status, "status", 's', CommandLineParser::Optional, "Dump status of rdm. Arg can be symbols or symbolNames." },
    { RClient::Diagnose, "diagnose", 0, CommandLineParser::Required, "Resend diagnostics for file." },
    { RClient::DiagnoseAll, "diagnose-all", 0, CommandLineParser::NoValue, "Resend diagnostics for all files." },
    { RClient::LastIndexed, "last-indexed", 0, CommandLineParser::NoValue, "Get timestamp of the last time indexing completed for the current project." },
    { RClient::IsIndexed, "is-indexed", 'T', CommandLineParser::Required, "Check if rtags knows about, and is ready to return information about, this source file." },
    { RClient::IsIndexing, "is-indexing", 0, CommandLineParser::Optional, "Check if rtags is currently indexing files in any project or in project matching pattern." },
    { RClient::HasFileManager, "has-filemanager", 0, CommandLineParser::Optional, "Check if rtags has info about files in this directory." },
    { RClient::PreprocessFile, "preprocess", 'E', CommandLineParser::Required, "Preprocess file." },
    { RClient::AsmFile, "asm", 0, CommandLineParser::Required, "Assemble file." },
    { RClient::Reindex, "reindex", 'V', CommandLineParser::Optional, "Reindex all files or all files matching pattern." },
    { RClient::CheckReindex, "check-reindex", 'x', CommandLineParser::Optional, "Check if reindexing is necessary for all files matching pattern." },
    { RClient::FindFile, "path", 'P', CommandLineParser::Optional, "Print files matching pattern." },
    { RClient::CurrentProject, "current-project", 0, CommandLineParser::NoValue, "Print path for current project." },
    { RClient::DumpFile, "dump-file", 'd', CommandLineParser::Required, "Dump source file." },
    { RClient::CheckIncludes, "check-includes", 0, CommandLineParser::Required, "Check includes for source file." },
    { RClient::DumpFileMaps, "dump-file-maps", 0, CommandLineParser::Required, "Dump file maps for file." },
    { RClient::GenerateTest, "generate-test", 0, CommandLineParser::Required, "Generate a test for a given source file." },
    { RClient::RdmLog, "rdm-log", 'g', CommandLineParser::NoValue, "Receive logs from rdm." },
    { RClient::FixIts, "fixits", 0, CommandLineParser::Required, "Get fixits for file." },
    { RClient::RemoveFile, "remove", 'D', CommandLineParser::Required, "Remove file from project." },
    { RClient::FindProjectRoot, "find-project-root", 0, CommandLineParser::Required, "Use to check behavior of find-project-root." },
    { RClient::FindProjectBuildRoot, "find-project-build-root", 0, CommandLineParser::Required, "Use to check behavior of find-project-root for builds." },
    { RClient::IncludeFile, "include-file", 0, CommandLineParser::Required, "Use to generate include statement for symbol." },
    { RClient::Sources, "sources", 0, CommandLineParser::Optional, "Dump sources for source file." },
    { RClient::Dependencies, "dependencies", 0, CommandLineParser::Required, "Dump dependencies for source file [(includes, included-by, depends-on, depended-on, tree-depends-on, raw)]." },
    { RClient::AllDependencies, "all-dependencies", 0, CommandLineParser::NoValue, "Dump dependencies for all source files [(includes, included-by, depends-on, depended-on, tree-depends-on, raw)]." },
    { RClient::ReloadFileManager, "reload-file-manager", 'B', CommandLineParser::NoValue, "Reload file manager." },
    { RClient::CodeCompleteAt, "code-complete-at", 'l', CommandLineParser::Required, "Code complete at location: arg is file:line:col." },
    { RClient::SendDiagnostics, "send-diagnostics", 0, CommandLineParser::Required, "Only for debugging. Send data to all -G connections." },
    { RClient::DumpCompletions, "dump-completions", 0, CommandLineParser::NoValue, "Dump cached completions." },
    { RClient::DumpCompileCommands, "dump-compile-commands", 0, CommandLineParser::NoValue, "Dump compilation database for project." },
    { RClient::SetBuffers, "set-buffers", 0, CommandLineParser::Optional, "Set active buffers (list of filenames for active buffers in editor)." },
    { RClient::ListBuffers, "list-buffers", 0, CommandLineParser::NoValue, "List active buffers." },
    { RClient::AddBuffers, "add-buffers", 0, CommandLineParser::Required, "Add additional buffers." },
    { RClient::RemoveBuffers, "remove-buffers", 0, CommandLineParser::Required, "Remove buffers." },
    { RClient::ListCursorKinds, "list-cursor-kinds", 0, CommandLineParser::NoValue, "List spelling for known cursor kinds." },
    { RClient::ClassHierarchy, "class-hierarchy", 0, CommandLineParser::Required, "Dump class hierarcy for struct/class at location." },
    { RClient::DebugLocations, "debug-locations", 0, CommandLineParser::Optional, "Manipulate debug locations." },
    { RClient::Validate, "validate", 0, CommandLineParser::NoValue, "Validate database files for current project." },
    { RClient::Tokens, "tokens", 0, CommandLineParser::Required, "Dump tokens for file. --tokens file.cpp:123-321 for range." },
    { RClient::DeadFunctions, "find-dead-functions", 0, CommandLineParser::Optional, "Find functions declared/defined in the current file that are never used in the project." },

    { RClient::None, String(), 0, CommandLineParser::NoValue, "" },
    { RClient::None, String(), 0, CommandLineParser::NoValue, "Command flags:" },
    { RClient::StripParen, "strip-paren", 'p', CommandLineParser::NoValue, "Strip parens in various contexts." },
    { RClient::Max, "max", 'M', CommandLineParser::Required, "Max lines of output for queries." },
    { RClient::ReverseSort, "reverse-sort", 'O', CommandLineParser::NoValue, "Sort output reversed." },
    { RClient::Rename, "rename", 0, CommandLineParser::NoValue, "Used for --references to indicate that we're using the results to rename symbols." },
    { RClient::UnsavedFile, "unsaved-file", 0, CommandLineParser::Required, "Pass unsaved file on command line. E.g. --unsaved-file=main.cpp:1200 then write 1200 bytes on stdin." },
    { RClient::LogFile, "log-file", 'L', CommandLineParser::Required, "Log to this file." },
    { RClient::NoContext, "no-context", 'N', CommandLineParser::NoValue, "Don't print context for locations." },
    { RClient::PathFilter, "path-filter", 'i', CommandLineParser::Required, "Filter out results not matching with arg." },
    { RClient::DependencyFilter, "dependency-filter", 0, CommandLineParser::Required, "Filter out results unless argument depends on them." },
    { RClient::RangeFilter, "range-filter", 0, CommandLineParser::Required, "Filter out results not in the specified range." },
    { RClient::FilterSystemHeaders, "filter-system-headers", 'H', CommandLineParser::NoValue, "Don't exempt system headers from path filters." },
    { RClient::AllReferences, "all-references", 'e', CommandLineParser::NoValue, "Include definitions/declarations/constructors/destructors for references. Used for rename symbol." },
    { RClient::TargetUsrs, "target-usrs", 0, CommandLineParser::NoValue, "Print all matching usr targets for -f. Used for debugging." },
    { RClient::AllTargets, "all-targets", 0, CommandLineParser::NoValue, "Print multiple targets for -f. Sorted by best match." },
    { RClient::Elisp, "elisp", 'Y', CommandLineParser::NoValue, "Output elisp: (list \"one\" \"two\" ...)." },
    { RClient::JSON, "json", 0, CommandLineParser::NoValue, "Output json." },
    { RClient::JSONDiagnosticsIncludeSkipped, "json-diagnostics-include-skipped", 0, CommandLineParser::NoValue, "Output json diagnostics with skipped ranges." },
    { RClient::Diagnostics, "diagnostics", 'm', CommandLineParser::NoValue, "Receive async formatted diagnostics from rdm." },
    { RClient::MatchRegex, "match-regexp", 'Z', CommandLineParser::NoValue, "Treat various text patterns as regexps (-P, -i, -V, -F)." },
    { RClient::MatchCaseInsensitive, "match-icase", 'I', CommandLineParser::NoValue, "Match case insensitively" },
    { RClient::AbsolutePath, "absolute-path", 'K', CommandLineParser::NoValue, "Print files with absolute path." },
    { RClient::SocketFile, "socket-file", 'n', CommandLineParser::Required, "Use this socket file (default is XDG_RUNTIME_DIR/rdm.socket else ~/.rdm)" },
    { RClient::SocketAddress, "socket-address", 0, CommandLineParser::Required, "Use this host:port combination (instead of --socket-file)." },
    { RClient::Timeout, "timeout", 'y', CommandLineParser::Required, "Max time in ms to wait for job to finish (default no timeout)." },
    { RClient::FindVirtuals, "find-virtuals", 'k', CommandLineParser::NoValue, "Use in combinations with -R or -r to show other implementations of this function." },
    { RClient::FindFilePreferExact, "find-file-prefer-exact", 'A', CommandLineParser::NoValue, "Use to make --find-file prefer exact matches over partial matches." },
    { RClient::SymbolInfoIncludeParents, "symbol-info-include-parents", 0, CommandLineParser::NoValue, "Use to make --symbol-info include parent symbols." },
    { RClient::SymbolInfoIncludeTargets, "symbol-info-include-targets", 0, CommandLineParser::NoValue, "Use to make --symbol-info include target symbols." },
    { RClient::SymbolInfoIncludeReferences, "symbol-info-include-references", 0, CommandLineParser::NoValue, "Use to make --symbol-info include reference symbols." },
    { RClient::SymbolInfoIncludeBaseClasses, "symbol-info-include-base-classes", 0, CommandLineParser::NoValue, "Use to make --symbol-info include baseclasses' symbols." },
    { RClient::SymbolInfoIncludeSourceCode, "symbol-info-include-source-code", 0, CommandLineParser::NoValue, "Use to make --symbol-info include source code." },
    { RClient::CursorKind, "cursor-kind", 0, CommandLineParser::NoValue, "Include cursor kind in --find-symbols output." },
    { RClient::DisplayName, "display-name", 0, CommandLineParser::NoValue, "Include display name in --find-symbols output." },
    { RClient::CurrentFile, "current-file", 0, CommandLineParser::Required, "Pass along which file is being edited to give rdm a better chance at picking the right project." },
    { RClient::DeclarationOnly, "declaration-only", 'G', CommandLineParser::NoValue, "Filter out definitions (unless inline).", },
    { RClient::DefinitionOnly, "definition-only", 0, CommandLineParser::NoValue, "Filter out declarations (unless inline).", },
    { RClient::KindFilter, "kind-filter", 0, CommandLineParser::Required, "Only return results matching this kind.", },
    { RClient::ContainingFunction, "containing-function", 'o', CommandLineParser::NoValue, "Include name of containing function in output."},
    { RClient::ContainingFunctionLocation, "containing-function-location", 0, CommandLineParser::NoValue, "Include location of containing function in output."},
    { RClient::BuildIndex, "build-index", 0, CommandLineParser::Required, "For sources with multiple builds, use the arg'th." },
    { RClient::CompilationFlagsOnly, "compilation-flags-only", 0, CommandLineParser::NoValue, "For --source, only print compilation flags." },
    { RClient::CompilationFlagsSplitLine, "compilation-flags-split-line", 0, CommandLineParser::NoValue, "For --source, print one compilation flag per line." },
    { RClient::CompilationFlagsPwd, "compilation-flags-pwd", 0, CommandLineParser::NoValue, "For --source, print pwd for compile command on the first line." },
    { RClient::DumpIncludeHeaders, "dump-include-headers", 0, CommandLineParser::NoValue, "For --dump-file, also dump dependencies." },
    { RClient::SilentQuery, "silent-query", 0, CommandLineParser::NoValue, "Don't log this request in rdm." },
    { RClient::SynchronousCompletions, "synchronous-completions", 0, CommandLineParser::NoValue, "Wait for completion results and print them to stdout." },
    { RClient::SynchronousDiagnostics, "synchronous-diagnostics", 0, CommandLineParser::NoValue, "Wait for diagnostics and print them to stdout." },
    { RClient::XML, "xml", 0, CommandLineParser::NoValue, "Output XML" },
    { RClient::NoSortReferencesByInput, "no-sort-references-by-input", 0, CommandLineParser::NoValue, "Don't sort references by input position." },
    { RClient::ProjectRoot, "project-root", 0, CommandLineParser::Required, "Override project root for compile commands." },
    { RClient::RTagsConfig, "rtags-config", 0, CommandLineParser::Required, "Print out .rtags-config for argument." },
    { RClient::WildcardSymbolNames, "wildcard-symbol-names", 'a', CommandLineParser::NoValue, "Expand * like wildcards in --list-symbols and --find-symbols." },
    { RClient::NoColor, "no-color", 'z', CommandLineParser::NoValue, "Don't colorize context." },
    { RClient::Wait, "wait", 0, CommandLineParser::NoValue, "Wait for reindexing to finish." },
    { RClient::CodeCompleteIncludeMacros, "code-complete-include-macros", 0, CommandLineParser::NoValue, "Include macros in code completion results." },
    { RClient::CodeCompleteIncludes, "code-complete-includes", 0, CommandLineParser::NoValue, "Give includes in completion results." },
    { RClient::CodeCompleteNoWait, "code-complete-no-wait", 0, CommandLineParser::NoValue, "Don't wait for synchronous completion if the translation unit has to be created." },
    { RClient::CodeCompletePrefix, "code-complete-prefix", 0, CommandLineParser::Required, "Filter out code completion results that don't start with this prefix." },
    { RClient::CodeCompletionEnabled, "code-completion-enabled", 'b', CommandLineParser::NoValue, "Inform rdm that we're code-completing. Use with --diagnose" },
    { RClient::NoSpellCheckinging, "no-spell-checking", 0, CommandLineParser::NoValue, "Don't produce spell check info in diagnostics." },
    { RClient::TokensIncludeSymbols, "tokens-include-symbols", 0, CommandLineParser::NoValue, "Include symbols for tokens." },
    { RClient::NoRealPath, "no-realpath", 0, CommandLineParser::NoValue, "Don't resolve paths using realpath(3)." },
    { RClient::IncludePath, "include-path", 0, CommandLineParser::Required, "Dump include path for symbol." },
    { RClient::MaxDepth, "max-depth", 0, CommandLineParser::Required, "Max search depth. Used for --include-path." },
    { RClient::None, String(), 0, CommandLineParser::NoValue, nullptr }
};

class RCCommand
{
public:
    RCCommand() {}
    virtual ~RCCommand() {}
    virtual RTags::ExitCode exec(RClient *rc, const std::shared_ptr<Connection> &connection) = 0;
    virtual String description() const = 0;
};

class QueryCommand : public RCCommand
{
public:
    QueryCommand(QueryMessage::Type t, String &&q)
        : RCCommand(), type(t), query(std::move(q))
    {}

    const QueryMessage::Type type;
    String query;
    Flags<QueryMessage::Flag> extraQueryFlags;

    virtual RTags::ExitCode exec(RClient *rc, const std::shared_ptr<Connection> &connection) override
    {
#ifndef HAS_JSON_H
        if (type == QueryMessage::CodeCompleteAt && rc->queryFlags() & QueryMessage::JSON) {
            fprintf(stdout, "{\"error\": \"JSON output for completions is not supported with this compiler\"}\n");
            return RTags::ArgumentParseError;
        }
#endif
        QueryMessage msg(type);
        msg.setCommandLine(rc->commandLine());
        msg.setQuery(std::move(query));
        msg.setBuildIndex(rc->buildIndex());
        msg.setUnsavedFiles(rc->unsavedFiles());
        msg.setFlags(extraQueryFlags | rc->queryFlags());
        msg.setMax(rc->max());
        msg.setPathFilters(rc->pathFilters());
        msg.setKindFilters(rc->kindFilters());
        msg.setRangeFilter(rc->minOffset(), rc->maxOffset());
        msg.setTerminalWidth(rc->terminalWidth());
        msg.setCurrentFile(rc->currentFile());
        msg.setCodeCompletePrefix(rc->codeCompletePrefix());
        msg.setMaxDepth(rc->maxDepth());
        return connection->send(msg) ? RTags::Success : RTags::NetworkFailure;
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

    virtual RTags::ExitCode exec(RClient *, const std::shared_ptr<Connection> &connection) override
    {
        const QuitMessage msg(mExitCode);
        return connection->send(msg) ? RTags::Success : RTags::NetworkFailure;
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
    virtual RTags::ExitCode exec(RClient *rc, const std::shared_ptr<Connection> &connection) override
    {
        const LogLevel level = mLevel == Default ? rc->logLevel() : mLevel;
        Flags<QueryMessage::Flag> flags = rc->queryFlags();
        if (!(rc->queryFlags() & (QueryMessage::Elisp|QueryMessage::XML|QueryMessage::JSON)))
            flags |= QueryMessage::XML;

        LogOutputMessage msg(level, flags);
        msg.setCommandLine(rc->commandLine());
        return connection->send(msg) ? RTags::Success : RTags::NetworkFailure;
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
    CompileCommand(String &&a, const Path &c)
        : RCCommand(), args(std::move(a)), cwd(c)
    {}
    CompileCommand(Path &&path)
        : RCCommand(), compileCommands(std::move(path))
    {}

    String args;
    Path cwd;
    Path compileCommands;
    virtual RTags::ExitCode exec(RClient *rc, const std::shared_ptr<Connection> &connection) override
    {
        IndexMessage msg;
        msg.setCommandLine(rc->commandLine());
        msg.setWorkingDirectory(std::move(cwd));
        msg.setFlag(IndexMessage::GuessFlags, rc->mGuessFlags);
        msg.setArguments(std::move(args));
        msg.setCompileCommands(std::move(compileCommands));
        msg.setEnvironment(rc->environment());
        if (!rc->projectRoot().isEmpty())
            msg.setProjectRoot(rc->projectRoot());

        return connection->send(msg) ? RTags::Success : RTags::NetworkFailure;
    }
    virtual String description() const override
    {
        return ("IndexMessage " + cwd);
    }
};

RClient::RClient()
    : mMax(-1), mMaxDepth(-1), mTimeout(-1), mMinOffset(-1), mMaxOffset(-1),
      mConnectTimeout(DEFAULT_CONNECT_TIMEOUT), mBuildIndex(0),
      mLogLevel(LogLevel::Error), mTcpPort(0), mGuessFlags(false),
      mTerminalWidth(-1), mExitCode(RTags::ArgumentParseError)
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

void RClient::addQuery(QueryMessage::Type type, String &&query, Flags<QueryMessage::Flag> extraQueryFlags)
{
    auto cmd = std::make_shared<QueryCommand>(type, std::move(query));
    cmd->extraQueryFlags = extraQueryFlags;
    mCommands.append(cmd);
}

void RClient::addQuitCommand(int exitCode)
{
    auto cmd = std::make_shared<QuitCommand>(exitCode);
    mCommands.append(cmd);
}

void RClient::addLog(LogLevel level)
{
    mCommands.append(std::make_shared<RdmLogCommand>(level));
}

void RClient::addCompile(String &&args, const Path &cwd)
{
    mCommands.append(std::make_shared<CompileCommand>(std::move(args), cwd));
}

void RClient::addCompile(Path &&path)
{
    mCommands.append(std::make_shared<CompileCommand>(std::move(path)));
}

void RClient::exec()
{
    RTags::initMessages();
    OnDestruction onDestruction([]() { Message::cleanup(); });
    std::shared_ptr<EventLoop> loop(new EventLoop);
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
            mExitCode = RTags::ConnectionFailure;
            return;
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
            mExitCode = RTags::ConnectionFailure;
            return;
        }
    } else if (!connection->connectUnix(mSocketFile, mConnectTimeout)) {
        if (mLogLevel >= LogLevel::Error)
            fprintf(stdout, "Can't seem to connect to server (%s)\n", mSocketFile.constData());
        mExitCode = RTags::ConnectionFailure;
        return;
    }

    for (int i=0; i<commandCount; ++i) {
        const std::shared_ptr<RCCommand> &cmd = mCommands.at(i);
        debug() << "running command " << cmd->description();
        mExitCode = cmd->exec(this, connection);
        if (mExitCode != RTags::Success) {
            break;
        } else if (loop->exec(timeout()) != EventLoop::Success) {
            mExitCode = RTags::TimeoutFailure;
            break;
        }
        mExitCode = connection->finishStatus();
    }
    if (connection->client())
        connection->client()->close();
    mCommands.clear();
}

CommandLineParser::ParseStatus RClient::parse(size_t argc, char **argv)
{
    Rct::findExecutablePath(*argv);
    const char *runtimeDir = getenv("XDG_RUNTIME_DIR");
    if (!runtimeDir) {
        mSocketFile = Path::home() + ".rdm";
    } else {
        mSocketFile = runtimeDir;
        mSocketFile += "/rdm.socket";
    }

    List<std::shared_ptr<QueryCommand> > projectCommands;

    Path logFile;
    Flags<LogFlag> logFlags = LogStderr;

    if (!isatty(STDOUT_FILENO)) {
        mQueryFlags |= QueryMessage::NoColor;
    }

    std::function<CommandLineParser::ParseStatus(RClient::OptionType type,
                                                 String &&value,
                                                 size_t &idx,
                                                 const List<String> &args)> cb;
    cb = [this, &projectCommands, &logFile](RClient::OptionType type, String &&value, size_t &idx, const List<String> &arguments) -> CommandLineParser::ParseStatus {
        switch (type) {
        case None:
        case NumOptions: {
            assert(0);
            break; }
        case Noop: {
            break; }
        case NoRealPath: {
            Path::setRealPathEnabled(false);
            break; }
        case Help: {
            CommandLineParser::help(stdout, "rc", opts);
            mExitCode = RTags::Success;
            return { String(), CommandLineParser::Parse_Ok } ; }
        case SocketFile: {
            mSocketFile = std::move(value);
            break; }
        case SocketAddress: {
            mTcpHost = std::move(value);
            const int colon = mTcpHost.lastIndexOf(':');
            if (colon == -1) {
                return { String::format<1024>("invalid --socket-address %s\n", mTcpHost.constData()), CommandLineParser::Parse_Error };
            }
            mTcpPort = atoi(mTcpHost.constData() + colon + 1);
            if (!mTcpPort) {
                return { String::format<1024>("invalid --socket-address %s", mTcpHost.constData()), CommandLineParser::Parse_Error };
            }
            mTcpHost.truncate(colon);
            break; }
        case GuessFlags: {
            mGuessFlags = true;
            break; }
        case Wait: {
            mQueryFlags |= QueryMessage::Wait;
            break; }
        case NoSpellCheckinging: {
            mQueryFlags |= QueryMessage::NoSpellChecking;
            break; }
        case CodeCompleteIncludeMacros: {
            mQueryFlags |= QueryMessage::CodeCompleteIncludeMacros;
            break; }
        case CodeCompletePrefix: {
            mCodeCompletePrefix = std::move(value);
            break; }
        case CodeCompleteIncludes: {
            mQueryFlags |= QueryMessage::CodeCompleteIncludes;
            break; }
        case CodeCompleteNoWait: {
            mQueryFlags |= QueryMessage::CodeCompleteNoWait;
            break; }
        case CodeCompletionEnabled: {
            mQueryFlags |= QueryMessage::CodeCompletionEnabled;
            break; }
        case CompilationFlagsOnly: {
            mQueryFlags |= QueryMessage::CompilationFlagsOnly;
            break; }
        case CompilationFlagsPwd: {
            mQueryFlags |= QueryMessage::CompilationFlagsPwd;
            break; }
        case CompilationFlagsSplitLine: {
            mQueryFlags |= QueryMessage::CompilationFlagsSplitLine;
            break; }
        case ContainingFunction: {
            mQueryFlags |= QueryMessage::ContainingFunction;
            break; }
        case ContainingFunctionLocation: {
            mQueryFlags |= QueryMessage::ContainingFunctionLocation;
            break; }
        case DeclarationOnly: {
            mQueryFlags |= QueryMessage::DeclarationOnly;
            break; }
        case DefinitionOnly: {
            mQueryFlags |= QueryMessage::DefinitionOnly;
            break; }
        case FindVirtuals: {
            mQueryFlags |= QueryMessage::FindVirtuals;
            break; }
        case FindFilePreferExact: {
            mQueryFlags |= QueryMessage::FindFilePreferExact;
            break; }
        case SymbolInfoIncludeParents: {
            mQueryFlags |= QueryMessage::SymbolInfoIncludeParents;
            break; }
        case SymbolInfoIncludeTargets: {
            mQueryFlags |= QueryMessage::SymbolInfoIncludeTargets;
            break; }
        case SymbolInfoIncludeReferences: {
            mQueryFlags |= QueryMessage::SymbolInfoIncludeReferences;
            break; }
        case SymbolInfoIncludeBaseClasses: {
            mQueryFlags |= QueryMessage::SymbolInfoIncludeBaseClasses;
            break; }
        case SymbolInfoIncludeSourceCode: {
            mQueryFlags |= QueryMessage::SymbolInfoIncludeSourceCode;
            break; }
        case CursorKind: {
            mQueryFlags |= QueryMessage::CursorKind;
            break; }
        case SynchronousCompletions: {
            mQueryFlags |= QueryMessage::SynchronousCompletions;
            break; }
        case SynchronousDiagnostics: {
            mQueryFlags |= QueryMessage::SynchronousDiagnostics;
            break; }
        case DisplayName: {
            mQueryFlags |= QueryMessage::DisplayName;
            break; }
        case AllReferences: {
            mQueryFlags |= QueryMessage::AllReferences;
            break; }
        case AllTargets: {
            mQueryFlags |= QueryMessage::AllTargets;
            break; }
        case TargetUsrs: {
            mQueryFlags |= QueryMessage::TargetUsrs;
            break; }
        case MatchCaseInsensitive: {
            mQueryFlags |= QueryMessage::MatchCaseInsensitive;
            break; }
        case MatchRegex: {
            mQueryFlags |= QueryMessage::MatchRegex;
            break; }
        case AbsolutePath: {
            mQueryFlags |= QueryMessage::AbsolutePath;
            break; }
        case ReverseSort: {
            mQueryFlags |= QueryMessage::ReverseSort;
            break; }
        case Rename: {
            mQueryFlags |= QueryMessage::Rename;
            break; }
        case Elisp: {
            mQueryFlags |= QueryMessage::Elisp;
            break; }
        case JSON: {
            mQueryFlags |= QueryMessage::JSON;
            break; }
        case JSONDiagnosticsIncludeSkipped: {
            mQueryFlags |= QueryMessage::JSONDiagnosticsIncludeSkipped;
            break; }
        case XML: {
            mQueryFlags |= QueryMessage::XML;
            break; }
        case FilterSystemHeaders: {
            mQueryFlags |= QueryMessage::FilterSystemIncludes;
            break; }
        case NoColor: {
            mQueryFlags |= QueryMessage::NoColor;
            break; }
        case NoContext: {
            mQueryFlags |= QueryMessage::NoContext;
            break; }
        case PathFilter: {
            mPathFilters.insert({ Path::resolved(value), QueryMessage::PathFilter::Self });
            break; }
        case DependencyFilter: {
            Path p = std::move(value);
            if (!p.isFile()) {
                return { String::format<1024>("%s doesn't seem to be a file", p.constData()), CommandLineParser::Parse_Error };
            }
            mPathFilters.insert({ Path::resolved(p), QueryMessage::PathFilter::Dependency });
            break; }
        case KindFilter: {
            mKindFilters.insert(value);
            break; }
        case WildcardSymbolNames: {
            mQueryFlags |= QueryMessage::WildcardSymbolNames;
            break; }
        case RangeFilter: {
            char *end;
            mMinOffset = strtoul(value.constData(), &end, 10);
            if (*end != '-') {
                return { String::format<1024>("Can't parse range, must be uint-uint. E.g. 1-123"), CommandLineParser::Parse_Error };
            }
            mMaxOffset = strtoul(end + 1, &end, 10);
            if (*end) {
                return { String::format<1024>("Can't parse range, must be uint-uint. E.g. 1-123"), CommandLineParser::Parse_Error };
            }
            if (mMaxOffset <= mMinOffset || mMinOffset < 0) {
                return { String::format<1024>("Invalid range (%d-%d), must be uint-uint. E.g. 1-123", mMinOffset, mMaxOffset),
                         CommandLineParser::Parse_Error };
            }
            break; }
        case Validate: {
            addQuery(QueryMessage::Validate);
            break; }
        case Version: {
            fprintf(stdout, "%s\n", RTags::versionString().constData());
            mExitCode = RTags::Success;
            return { String(), CommandLineParser::Parse_Ok }; }
        case VerifyVersion: {
            const int version = strtoul(value.constData(), nullptr, 10);
            if (version != NumOptions) {
                fprintf(stdout, "Protocol version mismatch got: %d expected: %d \n", version, NumOptions);
                mExitCode = RTags::ProtocolFailure;
                return { String(), CommandLineParser::Parse_Error };
            }
            break; }
        case Verbose: {
            ++mLogLevel;
            break; }
        case CodeCompleteAt: {
            String encoded = Location::encode(value);
            if (encoded.isEmpty()) {
                return { String::format<1024>("Can't resolve argument %s", value.constData()), CommandLineParser::Parse_Error };
            }

            addQuery(QueryMessage::CodeCompleteAt, std::move(encoded), QueryMessage::HasLocation);
            break; }
        case Silent: {
            mLogLevel = LogLevel::None;
            break; }
        case LogFile: {
            logFile = std::move(value);
            break; }
        case StripParen: {
            mQueryFlags |= QueryMessage::StripParentheses;
            break; }
        case DumpIncludeHeaders: {
            mQueryFlags |= QueryMessage::DumpIncludeHeaders;
            break; }
        case SilentQuery: {
            mQueryFlags |= QueryMessage::SilentQuery;
            break; }
        case BuildIndex: {
            bool ok;
            mBuildIndex = String(value).toULongLong(&ok);
            if (!ok) {
                return { String::format<1024>("--build-index [arg] must be >= 0"), CommandLineParser::Parse_Error };
            }
            break; }
        case ConnectTimeout: {
            mConnectTimeout = atoi(value.constData());
            if (mConnectTimeout < 0) {
                return { String::format<1024>("--connect-timeout [arg] must be >= 0"), CommandLineParser::Parse_Error };
            }
            break; }
        case Max: {
            bool ok;
            mMax = value.toULongLong(&ok);
            if (!ok) {
                return { String::format<1024>("-M [arg] must be >= 0"), CommandLineParser::Parse_Error };
            }
            break; }
        case Timeout: {
            mTimeout = atoi(value.constData());
            if (!mTimeout) {
                mTimeout = -1;
            } else if (mTimeout < 0) {
                return { String::format<1024>("-y [arg] must be >= 0"), CommandLineParser::Parse_Error };
            }
            break; }
        case UnsavedFile: {
            const size_t colon = value.lastIndexOf(':');
            if (colon == String::npos || colon + 1 == value.size()) {
                return { String::format<1024>("Can't parse -u [%s]", value.constData()), CommandLineParser::Parse_Error };
            }
            const Path path = value.left(colon);
            if (!path.isFile()) {
                return { String::format<1024>("Can't open [%s] for reading", path.nullTerminated()), CommandLineParser::Parse_Error };
            }

            FILE *f;
            Path unlinkFile;
            int bytes;
            if (std::isdigit(value.at(colon + 1))) {
                bytes = atoi(value.constData() + colon + 1);
                if (!bytes) {
                    return { String::format<1024>("Can't parse -u [%s]", value.constData()), CommandLineParser::Parse_Error };
                }
                f = stdin;
            } else {
                unlinkFile = value.mid(colon + 1);
                f = fopen(value.constData() + colon + 1, "r");
                if (!f) {
                    return { String::format<1024>("Can't open %s for reading", unlinkFile.constData()), CommandLineParser::Parse_Error };
                }
                bytes = Rct::fileSize(f);
            }

            String contents(bytes, '\0');
            const int r = fread(contents.data(), 1, bytes, f);
            if (!unlinkFile.isEmpty())
                Path::rm(unlinkFile);
            if (r != bytes) {
                return { String::format<1024>("Read error %d (%s). Got %d, expected %d", errno, Rct::strerror(errno).constData(), r, bytes), CommandLineParser::Parse_Error };
            }
            mUnsavedFiles[path] = std::move(contents);
            break; }
        case FollowLocation:
        case ClassHierarchy:
        case ReferenceLocation: {
            String encoded = Location::encode(value);
            if (encoded.isEmpty()) {
                return { String::format<1024>("Can't resolve argument %s", value.constData()), CommandLineParser::Parse_Error };
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
            addQuery(queryType, std::move(encoded), QueryMessage::HasLocation);
            break; }
        case SymbolInfo: {
            std::cmatch match;
            std::regex rx("^(.*):([0-9]+):([0-9]+):?-:?([0-9]+):([0-9]+):?(@[A-Za-z,]+)?", std::regex_constants::extended);
            Path path;
            List<String> kinds;
            uint32_t line = 0, col = 0, line2 = 0, col2 = 0;
            if (std::regex_match(value.constData(), match, rx)) {
                path.assign(value.constData(), match.length(1));
                line = atoi(value.constData() + match.position(2));
                col = atoi(value.constData() + match.position(3));
                line2 = atoi(value.constData() + match.position(4));
                col2 = atoi(value.constData() + match.position(5));
                if (match.length(6) > 1)
                    kinds = String(value.constData() + match.position(6) + 1, match.length(6) - 1).split(",");
                if (!line || !col || !line2 || !col2 || !path.resolve(Path::MakeAbsolute)) {
                    return { String::format<1024>("Can't parse range %s", value.constData()), CommandLineParser::Parse_Error };
                }
            } else {
                std::regex rx2("^(.*):([0-9]+):([0-9]+):?(@[A-Za-z,]+)?", std::regex_constants::basic);
                if (std::regex_match(value.constData(), match, rx2)) {
                    path.assign(value.constData(), match.length(1));
                    line = atoi(value.constData() + match.position(2));
                    col = atoi(value.constData() + match.position(3));
                    if (match.length(4) > 1)
                        kinds = String(value.constData() + match.position(4) + 1, match.length(4) - 1).split(",");
                    if (!line || !col || !path.resolve(Path::MakeAbsolute)) {
                        return { String::format<1024>("Can't parse range %s", value.constData()), CommandLineParser::Parse_Error };
                    }
                } else if (!Location::parse(value, Path(), Path::MakeAbsolute, &path, &line, &col)) {
                    return { String::format<1024>("Can't parse range %s", value.constData()), CommandLineParser::Parse_Error };
                }
            }
            for (String &k : kinds) {
                k.lowerCase();
            }
            String query;
            Serializer serializer(query);
            serializer << path << line << col << line2 << col2 << kinds;
            addQuery(QueryMessage::SymbolInfo, std::move(query));
            break; }
        case CurrentFile: {
            mCurrentFile = Path(value).resolved();
            break; }
        case ReloadFileManager: {
            addQuery(QueryMessage::ReloadFileManager);
            break; }
        case DumpCompletions: {
            addQuery(QueryMessage::DumpCompletions);
            break; }
        case DumpCompileCommands: {
            addQuery(QueryMessage::DumpCompileCommands);
            break; }
        case Clear: {
            addQuery(QueryMessage::ClearProjects);
            break; }
        case RdmLog: {
            addLog(RdmLogCommand::Default);
            break; }
        case Diagnostics: {
            addLog(RTags::DiagnosticsLevel);
            break; }
        case QuitRdm: {
            String arg;
            if (!value.isEmpty()) {
                arg = std::move(value);
            } else if (idx < arguments.size() && arguments[idx][0] != '-') {
                arg = arguments[idx++];
            }
            int exit = 0;
            if (!arg.isEmpty()) {
                bool ok;
                exit = String(arg).toLongLong(&ok);
                if (!ok) {
                    return { String::format<1024>("Invalid argument to -q"), CommandLineParser::Parse_Error };
                }
            }
            addQuitCommand(exit);
            break; }
        case DeleteProject: {
            addQuery(QueryMessage::DeleteProject, std::move(value));
            break; }
        case DebugLocations: {
            String arg;
            if (!value.isEmpty()) {
                arg = std::move(value);
            } else if (idx < arguments.size() && arguments[idx][0] != '-') {
                arg = arguments[idx++];
            }
            addQuery(QueryMessage::DebugLocations, std::move(arg));
            break; }
        case SendDiagnostics: {
            addQuery(QueryMessage::SendDiagnostics, std::move(value));
            break; }
        case FindProjectRoot: {
            const Path p = Path::resolved(value); // this won't work correctly with --no-realpath unless --no-realpath is passed first
            printf("findProjectRoot [%s] => [%s]\n", p.constData(), RTags::findProjectRoot(p, RTags::SourceRoot).constData());
            mExitCode = RTags::Success;
            return { String(), CommandLineParser::Parse_Ok }; }
        case FindProjectBuildRoot: {
            const Path p = Path::resolved(value); // this won't work correctly with --no-realpath unless --no-realpath is passed first
            printf("findProjectRoot [%s] => [%s]\n", p.constData(), RTags::findProjectRoot(p, RTags::BuildRoot).constData());
            mExitCode = RTags::Success;
            return { String(), CommandLineParser::Parse_Ok }; }
        case RTagsConfig: {
            const Path p = Path::resolved(value); // this won't work correctly with --no-realpath unless --no-realpath is passed first
            Map<String, String> config = RTags::rtagsConfig(p);
            printf("rtags-config: %s:\n", p.constData());
            for (const auto &it : config) {
                printf("%s: \"%s\"\n", it.first.constData(), it.second.constData());
            }
            mExitCode = RTags::Success;
            return { String(), CommandLineParser::Parse_Ok }; }
        case CurrentProject: {
            addQuery(QueryMessage::Project, String(), QueryMessage::CurrentProjectOnly);
            break; }
        case CheckReindex:
        case Reindex:
        case Project:
        case IsIndexing:
        case FindFile:
        case ListSymbols:
        case FindSymbols:
        case Sources:
        case IncludeFile:
        case JobCount:
        case Status: {
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
            case IsIndexing:
                queryType = QueryMessage::IsIndexing;
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

            String arg;
            if (!value.isEmpty()) {
                arg = std::move(value);
            } else if (idx < arguments.size() && arguments[idx][0] != '-') {
                arg = arguments[idx++];
            }
            if (!arg.isEmpty()) {
                Path p(arg);
                if (resolve && p.exists()) {
                    p.resolve();
                }
                if (resolve) {
                    addQuery(queryType, std::move(p));
                } else {
                    addQuery(queryType, std::move(arg));
                }
            } else {
                addQuery(queryType, String());
            }
            assert(!mCommands.isEmpty());
            if (queryType == QueryMessage::Project)
                projectCommands.append(std::static_pointer_cast<QueryCommand>(mCommands.back()));
            break; }
        case ListBuffers: {
            addQuery(QueryMessage::SetBuffers);
            break; }
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
            mExitCode = RTags::Success;
            return { String(), CommandLineParser::Parse_Ok }; }
        case SetBuffers:
        case AddBuffers:
        case RemoveBuffers: {
            String arg;
            if (!value.isEmpty()) {
                arg = std::move(value);
            } else if (idx < arguments.size() && (arguments[idx][0] != '-' || arguments[idx] == "-")) {
                arg = arguments[idx++];
            }
            String encoded;
            if (!arg.isEmpty()) {
                Hash<Path, bool> paths;
                auto addBuffer = [&paths](const String &p) {
                    if (p.isEmpty())
                        return;
                    bool active = true;
                    Path path;
                    if (p.startsWith("\\")) {
                        path = p.mid(1);
                    } else if (p.startsWith("+")) {
                        path = p.mid(1);
                    } else if (p.startsWith("-")) {
                        path = p.mid(1);
                        active = false;
                    } else {
                        path = p;
                    }
                    if (path.resolve() && path.isFile()) {
                        paths[path] = active;
                    } else {
                        fprintf(stderr, "\"%s\" doesn't seem to be a file.\n", p.constData());
                    }
                };

                if (arg == "-") {
                    char buf[1024];
                    while (fgets(buf, sizeof(buf), stdin)) {
                        String a(buf);
                        if (a.endsWith('\n'))
                            a.chop(1);
                        addBuffer(a);
                    }
                } else {
                    for (const String &buffer : arg.split(';')) {
                        addBuffer(buffer);
                    }
                }
                Serializer serializer(encoded);
                serializer << '1'; // version marker to not crash with older versions of rc
                switch (type) {
                case AddBuffers: serializer << 1; break;
                case SetBuffers: serializer << 0; break;
                case RemoveBuffers: serializer << -1; break;
                default: assert(0); break;
                }
                serializer << paths;
            }
            addQuery(QueryMessage::SetBuffers, std::move(encoded));
            break; }
        case LoadCompileCommands: {
            Path path;
            if (!value.isEmpty()) {
                path = std::move(value);
            } else if (idx < arguments.size() && arguments[idx][0] != '-') {
                path = arguments[idx++];
            } else {
                path = Path::pwd();
            }
            path.resolve(Path::MakeAbsolute);
            if (!path.exists()) {
                return { String::format<1024>("%s does not seem to exist", path.constData()), CommandLineParser::Parse_Error };
            } else if (path.isDir()) {
                path = path.ensureTrailingSlash() + "compile_commands.json";
            } else if (!path.endsWith("/compile_commands.json")) {
                return { "The file has to be called compile_commands.json", CommandLineParser::Parse_Error };
            }
            if (!path.exists()) {
                return { String::format<1024>("no compile_commands.json file in %s", path.constData()), CommandLineParser::Parse_Error };
            }

            addCompile(std::move(path));
            break; }
        case HasFileManager: {
            Path p;
            if (!value.isEmpty()) {
                p = std::move(value);
            } else if (idx < arguments.size() && arguments[idx][0] != '-') {
                p = arguments[idx++];
            } else {
                p = ".";
            }
            p.resolve(Path::MakeAbsolute);
            if (!p.exists()) {
                return { String::format<1024>("%s does not seem to exist", p.constData()), CommandLineParser::Parse_Error };
            }
            if (p.isDir() && !p.endsWith('/'))
                p.append('/');
            addQuery(QueryMessage::HasFileManager, std::move(p), QueryMessage::HasMatch);
            break; }
        case ProjectRoot: {
            Path p = std::move(value);
            if (!p.isDir()) {
                return { String::format<1024>("%s does not seem to be a directory", p.constData()), CommandLineParser::Parse_Error };
            }

            p.resolve(Path::MakeAbsolute);
            mProjectRoot = p.ensureTrailingSlash();
            break; }
        case Suspend: {
            Path p;
            if (!value.isEmpty()) {
                p = std::move(value);
            } else if (idx < arguments.size() && arguments[idx][0] != '-') {
                p = arguments[idx++];
            }
            String change;
            if (!p.isEmpty() && p != "clear" && p != "all") {
                p.resolve(Path::MakeAbsolute);
                if (!p.isFile()) {
                    return { String::format<1024>("%s is not a file", p.constData()), CommandLineParser::Parse_Error };
                }
                if (idx + 1 < arguments.size()) {
                    if (arguments[idx + 1] == "on" || arguments[idx + 1] == "off") {
                        change = std::move(arguments[++idx]);
                    } else if (arguments[idx + 1] == "toggle") {
                        ++idx;
                    }
                }
            }
            String data;
            Serializer serializer(data);
            serializer << p << change;
            addQuery(QueryMessage::Suspend, std::move(data));
            break; }
        case Compile: {
            auto quote = [](const String &str) -> String {
                if (str.contains(' ')) {
                    String ret = str;
                    ret.replace("\"", "\\\"");
                    ret.insert(0, "\"");
                    ret.append("\"");
                    return ret;
                }
                return str;
            };
            String args;
            if (idx + 1 < arguments.size()) {
                args = quote(std::move(value));
                while (++idx < arguments.size()) {
                    if (!args.isEmpty())
                        args.append(' ');
                    args.append(quote(arguments[idx]));
                }
            } else {
                args = std::move(value);
            }
            if (args == "-" || args.isEmpty()) {
                String pending;
                char buf[16384];
                while (fgets(buf, sizeof(buf), stdin)) {
                    pending += buf;
                    if (!pending.endsWith("\\\n")) {
                        addCompile(std::move(pending), Path::pwd());
                    } else {
                        memset(pending.data() + pending.size() - 2, ' ', 2);
                    }
                }
                if (!pending.isEmpty()) {
                    addCompile(std::move(pending), Path::pwd());
                }
            } else {
                addCompile(std::move(args), Path::pwd());
            }
            break; }
        case LastIndexed: {
            addQuery(QueryMessage::LastIndexed);
            break; }
        case NoSortReferencesByInput: {
            mQueryFlags |= QueryMessage::NoSortReferencesByInput;
            break; }
        case DiagnoseAll: {
            addQuery(QueryMessage::Diagnose, String());
            break; }
        case IsIndexed:
        case DumpFile:
        case CheckIncludes:
        case GenerateTest:
        case Diagnose:
        case DeadFunctions:
        case FixIts: {
            Path p = std::move(value);
            if (!p.exists() && (!p.isEmpty() || type != DeadFunctions)) {
                return { String::format<1024>("%s does not exist", p.constData()), CommandLineParser::Parse_Error };
            }

            if (!p.isAbsolute() && !p.isEmpty())
                p.prepend(Path::pwd());

            if (p.isDir()) {
                if (type != IsIndexed) {
                    return { String::format<1024>("%s is not a file", p.constData()), CommandLineParser::Parse_Error };
                } else if (!p.endsWith('/')) {
                    p.append('/');
                }
            }
            Flags<QueryMessage::Flag> extraQueryFlags;
            if (!p.isEmpty()) {
                p.resolve();
                extraQueryFlags |= QueryMessage::HasMatch;
            }
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
            case DeadFunctions:
                queryType = QueryMessage::DeadFunctions;
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

            addQuery(queryType, std::move(p), extraQueryFlags);
            break; }
        case AllDependencies: {
            String encoded;
            List<String> args;
            while (idx < arguments.size() && arguments[idx][0] != '-') {
                args.append(arguments[idx++]);
            }
            Serializer s(encoded);
            s << Path() << args;
            addQuery(QueryMessage::Dependencies, std::move(encoded));
            break; }
        case DumpFileMaps:
        case Dependencies: {
            Path p = std::move(value);
            if (!p.isFile()) {
                return { String::format<1024>("%s is not a file", p.constData()), CommandLineParser::Parse_Error };
            }
            p.resolve();
            List<String> args;
            while (idx + 1 < arguments.size() && arguments[idx + 1][0] != '-') {
                args.append(arguments[++idx]);
            }

            String encoded;
            Serializer s(encoded);
            s << p << args;
            addQuery(type == DumpFileMaps ? QueryMessage::DumpFileMaps : QueryMessage::Dependencies, std::move(encoded));
            break; }
        case Tokens: {
            char path[PATH_MAX];
            uint32_t from, to;
            if (sscanf(value.constData(), "%[^':']:%u-%u", path, &from, &to) != 3) {
                if (sscanf(value.constData(), "%[^':']:%u-", path, &from) == 2) {
                    to = UINT_MAX;
                } else if (sscanf(value.constData(), "%[^':']:-%u", path, &to) == 2) {
                    from = 0;
                } else {
                    strncpy(path, value.constData(), value.size());
                    from = 0;
                    to = UINT_MAX;
                }
            }

            const Path p = Path::resolved(path);
            if (!p.isFile()) {
                return { String::format<1024>("%s is not a file", value.constData()), CommandLineParser::Parse_Error };
            }
            if (from >= to) {
                return { String::format<1024>("Invalid range: %s", value.constData()), CommandLineParser::Parse_Error };
            }
            String data;
            Serializer s(data);
            s << p << from << to;
            addQuery(QueryMessage::Tokens, std::move(data));
            break; }
        case TokensIncludeSymbols: {
            mQueryFlags |= QueryMessage::TokensIncludeSymbols;
            break; }
        case PreprocessFile: {
            Path p = std::move(value);
            p.resolve(Path::MakeAbsolute);
            if (!p.isFile()) {
                return { String::format<1024>("%s is not a file", p.constData()), CommandLineParser::Parse_Error };
            }
            addQuery(QueryMessage::PreprocessFile, std::move(p), QueryMessage::HasMatch);
            break; }
        case AsmFile: {
            Path p = std::move(value);
            p.resolve(Path::MakeAbsolute);
            if (!p.isFile()) {
                return { String::format<1024>("%s is not a file", p.constData()), CommandLineParser::Parse_Error };
            }
            addQuery(QueryMessage::AsmFile, std::move(p), QueryMessage::HasMatch);
            break; }
        case RemoveFile: {
            Path p = Path::resolved(value, Path::MakeAbsolute);
            if (!p.exists()) {
                addQuery(QueryMessage::RemoveFile, std::move(p), QueryMessage::HasMatch);
            } else {
                addQuery(QueryMessage::RemoveFile, std::move(value), QueryMessage::HasMatch);
            }
            break; }
        case ReferenceName: {
            addQuery(QueryMessage::ReferencesName, std::move(value));
            break; }
        case IncludePath: {
            String encoded = Location::encode(value);
            if (encoded.isEmpty()) {
                return { String::format<1024>("include path can't resolve argument %s", value.constData()), CommandLineParser::Parse_Error };
            }

            addQuery(QueryMessage::IncludePath, std::move(encoded), QueryMessage::HasLocation);
            break; }
        case MaxDepth: {
            const int depth = atoi(value.constData());
            if (depth <= 0) {
                return { String::format<1024>("Invalid depth %s", value.constData()), CommandLineParser::Parse_Error };
            }

            mMaxDepth = depth;
            break; }

        }
        return { String(), CommandLineParser::Parse_Exec };
    };

    const std::initializer_list<CommandLineParser::Option<CommandLineParser::ConfigOptionType> > configOpts = {
        { CommandLineParser::Config, "config", 0, CommandLineParser::Required, "Use this file (instead of ~/.rcrc)." },
        { CommandLineParser::NoRc, "no-rc", 0, CommandLineParser::NoValue, "Don't load any rc files." }
    };

    auto ret = CommandLineParser::parse<OptionType>(argc, argv, opts, NullFlags, cb, "rc", configOpts, &mCommandLine);
    if (ret.status == CommandLineParser::Parse_Error) {
        ret.error += "\nTry 'rc --help' for more information.";
    }
    if (ret.status != CommandLineParser::Parse_Exec)
        return ret;

    if (!initLogging(argv[0], logFlags, mLogLevel, logFile)) {
        return { String::format<1024>("Can't initialize logging with %d %s %s", mLogLevel.toInt(), logFile.constData(), logFlags.toString().constData()), CommandLineParser::Parse_Error };
    }

    if (mCommands.isEmpty()) {
        help(stderr, argv[0], opts);
        return { "No commands", CommandLineParser::Parse_Error };
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
        for (size_t i = 0; i < argc; ++i)
            l << " " << argv[i];
    }

    return { String(), CommandLineParser::Parse_Exec };
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
