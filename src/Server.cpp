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

#include "Server.h"

#include <clang-c/Index.h>
#include <clang-c/CXCompilationDatabase.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <cstdint>
#include <iterator>
#include <map>
#include <unordered_map>
#include <vector>

#include "TokensJob.h"
#include "ClassHierarchyJob.h"
#include "CompletionThread.h"
#include "IncludePathJob.h"
#include "DependenciesJob.h"
#include "ClangThread.h"
#include "FileManager.h"
#include "Filter.h"
#include "FindFileJob.h"
#include "FindSymbolsJob.h"
#include "FollowLocationJob.h"
#include "IncludeFileJob.h"
#include "IndexDataMessage.h"
#include "IndexMessage.h"
#include "JobScheduler.h"
#include "ListSymbolsJob.h"
#include "LogOutputMessage.h"
#include "Match.h"
#include "Preprocessor.h"
#include "Project.h"
#include "QueryMessage.h"
#include "RClient.h"
#include "IndexParseData.h"
#include "rct/Connection.h"
#include "rct/DataFile.h"
#include "rct/EventLoop.h"
#include "rct/Log.h"
#include "rct/Message.h"
#include "rct/Path.h"
#include "rct/Process.h"
#include "rct/QuitMessage.h"
#include "rct/Rct.h"
#include "rct/SocketClient.h"
#include "rct/Value.h"
#include "ReferencesJob.h"
#include "RTags.h"
#include "RTagsLogOutput.h"
#include "Source.h"
#include "StatusJob.h"
#include "SymbolInfoJob.h"
#include "VisitFileMessage.h"
#include "VisitFileResponseMessage.h"
#include "RTagsVersion.h"
#include "FileMap.h"
#include "Location.h"
#include "QueryJob.h"
#include "Sandbox.h"
#include "Symbol.h"
#include "clang-c/CXString.h"
#include "rct/FinishMessage.h"
#include "rct/Map.h"
#include "rct/ResponseMessage.h"
#include "rct/Serializer.h"
#include "rct/SocketServer.h"
#include "rct/Thread.h"

#define TO_STR1(x) #x
#define TO_STR(x) TO_STR1(x)
#define CLANG_LIBDIR_STR TO_STR(CLANG_LIBDIR)
#ifdef CLANG_INCLUDE
#define CLANG_INCLUDE_STR TO_STR(CLANG_INCLUDE)
#endif
#ifdef CLANG_VERSION
#define CLANG_VERSION_STRING TO_STR(CLANG_VERSION)
#endif


// Absolute paths to search (under) for (clang) system include files
// Iterate until we find a dir at <abspath>/clang/<version>/include.
// As of clang 4.0.0 we don't need (and can't have) these includes on Mac.
#if CINDEX_VERSION_ENCODE(CINDEX_VERSION_MAJOR, CINDEX_VERSION_MINOR) >= CINDEX_VERSION_ENCODE(0, 37) && defined(OS_Darwin)
static const List<Path> sSystemIncludePaths;
#else
static const List<Path> sSystemIncludePaths = {
    CLANG_LIBDIR_STR, // standard llvm build, debian/ubuntu
    "/usr/lib"        // fedora, arch
};
#endif

Server *Server::sInstance = nullptr;
Server::Server()
    : mSuspended(false), mEnvironment(Rct::environment()), mPollTimer(-1), mExitCode(0),
      mLastFileId(0), mCompletionThread(nullptr), mActiveBuffersSet(false)
{
    assert(!sInstance);
    sInstance = this;
}

Server::~Server()
{
    if (mPollTimer >= 0)
        EventLoop::eventLoop()->unregisterTimer(mPollTimer);

    if (mCompletionThread) {
        mCompletionThread->stop();
        mCompletionThread->join();
        delete mCompletionThread;
        mCompletionThread = nullptr;
    }

    stopServers();
    mProjects.clear(); // need to be destroyed before sInstance is set to 0
    assert(sInstance == this);
    sInstance = nullptr;
    Message::cleanup();
}

bool Server::init(const Options &options)
{
    RTags::initMessages();

    Sandbox::setRoot(options.sandboxRoot);

    mOptions = options;
    mSuspended = (options.options & StartSuspended);
    mOptions.defaultArguments << String::format<32>("-ferror-limit=%d", mOptions.errorLimit);
    if (options.options & Wall)
        mOptions.defaultArguments << "-Wall";
    if (options.options & Weverything)
        mOptions.defaultArguments << "-Weverything";
    if (options.options & SpellChecking)
        mOptions.defaultArguments << "-fspell-checking";
    if (!(options.options & NoNoUnknownWarningsOption))
        mOptions.defaultArguments.append("-Wno-unknown-warning-option");

    mOptions.defines << Source::Define("RTAGS", String(), Source::Define::NoValue);

    if (mOptions.options & EnableCompilerManager) {
#ifndef OS_Darwin   // this causes problems on MacOS+clang
        // http://clang.llvm.org/compatibility.html#vector_builtins
        const char *gccBuiltIntVectorFunctionDefines[] = {
            "__builtin_ia32_rolhi",
            "__builtin_ia32_pause",
            "__builtin_ia32_addcarryx_u32",
            "__builtin_ia32_bsrsi",
            "__builtin_ia32_rdpmc",
            "__builtin_ia32_rdtsc",
            "__builtin_ia32_rdtscp",
            "__builtin_ia32_rolqi",
            "__builtin_ia32_rorqi",
            "__builtin_ia32_rorhi",
            "__builtin_ia32_rolhi",
            "__builtin_ia32_rdseed_di_step",
            "__builtin_ia32_xsaveopt",
            "__builtin_ia32_xsaveopt64",
            "__builtin_ia32_sbb_u32",
            nullptr
        };
        for (int i=0; gccBuiltIntVectorFunctionDefines[i]; ++i) {
            mOptions.defines << Source::Define(String::format<128>("%s(...)", gccBuiltIntVectorFunctionDefines[i]));
        }
#endif
    }
#ifdef CLANG_INCLUDE
    mOptions.includePaths.append(Source::Include(Source::Include::Type_System, CLANG_INCLUDE_STR));
#endif

    if (!(mOptions.options & NoLibClangIncludePath)) {
        // Iterate until we find an existing directory
        for (Path systemInclude : sSystemIncludePaths) {
            systemInclude = systemInclude.ensureTrailingSlash();
            systemInclude << "clang/" << CLANG_VERSION_STRING << "/include/";
            if (systemInclude.isDir()) {
                mOptions.includePaths.append(Source::Include(Source::Include::Type_System, systemInclude));
                break;
            }
        }
    }

    if (!initServers()) {
        error("Unable to listen on %s (errno: %d)", mOptions.socketFile.constData(), errno);
        return false;
    }

    mDefaultJobCount = options.jobCount;
    {
        Log l(LogLevel::Error, LogOutput::StdOut|LogOutput::TrailingNewLine);
        l << "Running with" << mOptions.jobCount << "jobs, using args:"
          << String::join(mOptions.defaultArguments, ' ');
        if (!mOptions.includePaths.isEmpty()) {
            l << "\nIncludepaths:";
            for (const auto &inc : mOptions.includePaths)
                l << inc.toString();
        }
    }

    if (mOptions.options & ClearProjects) {
        clearProjects(Clear_All);
    }

    mJobScheduler.reset(new JobScheduler);
    if (!mJobScheduler->start()) {
        error() << "Failed to start job scheduler";
        return false;
    }

    if (!load())
        return false;
    if (!(mOptions.options & NoStartupCurrentProject)) {
        Path current = Path(mOptions.dataDir + ".currentProject").readAll(1024);
        RTags::decodePath(current);
        if (current.size() > 1) {
            current.chop(1);
            const auto project = mProjects.value(current);
            if (!project) {
                error() << "Can't restore current project" << current;
                unlink((mOptions.dataDir + ".currentProject").constData());
            } else {
                setCurrentProject(project);
            }
        }
    }

    assert(mOptions.pollTimer >= 0);
    if (mOptions.pollTimer) {
        mPollTimer = EventLoop::eventLoop()->registerTimer([this](int) {
                for (auto proj : mProjects) {
                    proj.second->validateAll();
                }
            }, mOptions.pollTimer * 1000);
    }
    return true;
}

bool Server::initServers()
{
    if (mOptions.tcpPort) {
        for (int i=0; i<10; ++i) {
            mTcpServer.reset(new SocketServer);
            warning() << "listening" << mOptions.tcpPort;
            if (mTcpServer->listen(mOptions.tcpPort)) {
                break;
            }
            mTcpServer.reset();
            if (!i) {
                enum { Timeout = 1000 };
                std::shared_ptr<Connection> connection = Connection::create(RClient::NumOptions);
                if (connection->connectTcp("127.0.0.1", mOptions.tcpPort, Timeout)) {
                    connection->send(QuitMessage());
                    connection->disconnected().connect(std::bind([](){ EventLoop::eventLoop()->quit(); }));
                    connection->finished().connect(std::bind([](){ EventLoop::eventLoop()->quit(); }));
                    EventLoop::eventLoop()->exec(Timeout);
                }
            } else {
                sleep(1);
            }
        }
        if (!mTcpServer)
            return false;
        mTcpServer->newConnection().connect(std::bind(&Server::onNewConnection, this, std::placeholders::_1));
    }

#ifdef RTAGS_HAS_LAUNCHD
    // If Launchd, it goes into this bit and never comes out.
    if (mOptions.options & Launchd) {
        warning("initServers: launchd mode.");

        int *fds = 0;
        size_t numFDs;
        int ret = launch_activate_socket("Listener", &fds, &numFDs);

        if (ret != 0) {
            error("Failed to retrieve launchd socket: %s", strerror(ret));
        } else if (numFDs != 1) {
            error("Unexpected number of sockets from launch_activate_socket: %zu", numFDs);
        } else {
            warning() << "got fd from launchd: " << fds[0];
            mUnixServer.reset(new SocketServer);
            if (!mUnixServer->listenFD(fds[0]))
                mUnixServer.reset();
        }

        free(fds);
        if (!mUnixServer)
            return false;
        mUnixServer->newConnection().connect(std::bind(&Server::onNewConnection, this, std::placeholders::_1));
        return true;
    }
#endif

    char *listenFds = getenv("LISTEN_FDS");
    if (listenFds != nullptr) {
        auto numFDs = atoi(listenFds);
        if (numFDs != 1) {
            error("Unexpected number of sockets from systemd: %d", numFDs);
            return false;
        }

        mUnixServer.reset(new SocketServer);

        if (!mUnixServer->listenFD(3)) {
            return false;
        }

        mUnixServer->newConnection().connect(std::bind(&Server::onNewConnection, this, std::placeholders::_1));
        return true;
    }

    if (Path::exists(mOptions.socketFile)) {
        enum { Timeout = 1000 };
        std::shared_ptr<Connection> connection = Connection::create(RClient::NumOptions);
        if (connection->connectUnix(mOptions.socketFile, Timeout)) {
            connection->send(QuitMessage());
            connection->disconnected().connect(std::bind([](){ EventLoop::eventLoop()->quit(); }));
            connection->finished().connect(std::bind([](){ EventLoop::eventLoop()->quit(); }));
            EventLoop::eventLoop()->exec(Timeout);
            sleep(1);
        }

        Path::rm(mOptions.socketFile);
    }

    mUnixServer.reset(new SocketServer);
    warning() << "listening" << mOptions.socketFile;
    if (!mUnixServer->listen(mOptions.socketFile)) {
        error() << "Failed to listen on " << mOptions.socketFile;
        return false;
    }

    mUnixServer->newConnection().connect(std::bind(&Server::onNewConnection, this, std::placeholders::_1));
    return true;
}

std::shared_ptr<Project> Server::addProject(const Path &path)
{
    std::shared_ptr<Project> &project = mProjects[path];
    if (!project) {
        project.reset(new Project(path));
        if (!project->init()) {
            Path::rmdir(project->projectDataDir());
            mProjects.erase(path);
            return std::shared_ptr<Project>();
        }
    }
    return project;
}

void Server::onNewConnection(SocketServer *server)
{
    while (true) {
        std::shared_ptr<SocketClient> client = server->nextConnection();
        if (!client) {
            break;
        }
        std::shared_ptr<Connection> conn = Connection::create(client, RClient::NumOptions);
        if (mOptions.maxSocketWriteBufferSize) {
            client->setMaxWriteBufferSize(mOptions.maxSocketWriteBufferSize);
        }
        conn->setErrorHandler([](const std::shared_ptr<SocketClient> &, Message::MessageError &&error) {
                if (error.type == Message::Message_VersionError) {
                    ::error("Wrong version marker. You're probably using mismatched versions of rc and rdm");
                } else {
                    logDirect(LogLevel::Error, error.text);
                }
            });
        conn->newMessage().connect(std::bind(&Server::onNewMessage, this, std::placeholders::_1, std::placeholders::_2));
        mConnections.insert(conn);
        std::weak_ptr<Connection> weak = conn;
        conn->disconnected().connect(std::bind([this, weak]() {
                    if (std::shared_ptr<Connection> c = weak.lock()) {
                        c->disconnected().disconnect();
                        mConnections.remove(c);
                    }
                }));
    }
}

void Server::onNewMessage(const std::shared_ptr<Message> &message, const std::shared_ptr<Connection> &connection)
{
    switch (message->messageId()) {
    case IndexMessage::MessageId:
        handleIndexMessage(std::static_pointer_cast<IndexMessage>(message), connection);
        break;
    case QueryMessage::MessageId:
        handleQueryMessage(std::static_pointer_cast<QueryMessage>(message), connection);
        break;
    case QuitMessage::MessageId:
        mExitCode = std::static_pointer_cast<QuitMessage>(message)->exitCode();
        EventLoop::eventLoop()->quit();
        connection->finish("Shutting down");
        break;
    case IndexDataMessage::MessageId:
        handleIndexDataMessage(std::static_pointer_cast<IndexDataMessage>(message), connection);
        break;
    case LogOutputMessage::MessageId: {
        auto msg = std::static_pointer_cast<LogOutputMessage>(message);
        logDirect(LogLevel::Error, msg->commandLine(), LogOutput::StdOut|LogOutput::TrailingNewLine);
        handleLogOutputMessage(msg, connection);
        break; }
    case VisitFileMessage::MessageId:
        handleVisitFileMessage(std::static_pointer_cast<VisitFileMessage>(message), connection);
        break;
    case ResponseMessage::MessageId:
    case FinishMessage::MessageId:
    case VisitFileResponseMessage::MessageId:
        error() << "Unexpected message" << static_cast<int>(message->messageId());
        // assert(0);
        connection->finish(RTags::UnexpectedMessageError);
        break;
    default:
        error("Unknown message: %d", message->messageId());
        connection->finish(RTags::UnknownMessageError);
        break;
    }
    if ((mOptions.options & (NoFileManagerWatch|NoFileManager)) == NoFileManagerWatch) {
        std::shared_ptr<Project> project = currentProject();
        if (project && project->fileManager() && (Rct::monoMs() - project->fileManager()->lastReloadTime()) > 60000)
            project->fileManager()->load(FileManager::Asynchronous);
    }
}

String Server::guessArguments(const String &args, const Path &pwd, const Path &projectRootOverride) const
{
    Set<Path> includePaths;
    List<String> ret;
    bool hasInput = false;
    Set<Path> roots;
    if (!projectRootOverride.isEmpty())
        roots.insert(projectRootOverride.ensureTrailingSlash());
    ret << "/usr/bin/g++"; // this should be clang on mac
    const List<String> split = args.split(" ");
    for (size_t i=0; i<split.size(); ++i) {
        const String &s = split.at(i);
        if (s == "--build-root") {
            const Path root = split.value(++i);
            if (!root.isEmpty())
                roots.insert(root.ensureTrailingSlash());
            continue;
        }
        Path file = s;
        if (!file.isAbsolute())
            file.prepend(pwd);
        if (file.isFile()) {
            if (!hasInput) {
                hasInput = true;
                ret << file;
                includePaths.insert(file.parentDir());
                const Path projectRoot = RTags::findProjectRoot(file, RTags::SourceRoot);
                if (!projectRoot.isEmpty())
                    roots.insert(projectRoot);
            } else {
                return String();
            }
        } else {
            ret << s;
        }

    }
    if (!hasInput) {
        return String();
    }

    std::function<void(const Path &, const Path &)> process = [&](const Path &root, const Path &path) {
        for (const Path &maybeHeader : path.files(Path::File)) {
            if (maybeHeader.isHeader()) {
                Path p = path;
                do {
                    assert(!p.isEmpty());
                    if (!includePaths.insert(p) || p == root)
                        break;
                    p = p.parentDir();
                } while (!p.isEmpty());
                break;
            }
        }
        for (const Path &dir : path.files(Path::Directory)) {
            process(root, dir);
        }
    };

    for (const Path &root : roots)
        process(root, root);
    for (const Path &p : includePaths) {
        assert(!p.isEmpty());
        ret << ("-I" + p);
    }

    return String::join(ret, ' ');
}

bool Server::loadCompileCommands(IndexParseData &data, const Path &compileCommands, const List<String> &environment, SourceCache *cache) const
{
    if (Sandbox::hasRoot() && !data.project.isEmpty() && !data.project.startsWith(Sandbox::root())) {
        error("Invalid --project-root '%s', must be inside --sandbox-root '%s'",
              data.project.constData(), Sandbox::root().constData());
        return false;
    }

    CXCompilationDatabase_Error err;
    CXCompilationDatabase db = clang_CompilationDatabase_fromDirectory(compileCommands.parentDir().constData(), &err);
    if (err != CXCompilationDatabase_NoError) {
        error("Can't load compilation database from %s", compileCommands.constData());
        return false;
    }
    const uint32_t fileId = Location::insertFile(compileCommands);
    bool ret = false;
    CXCompileCommands cmds = clang_CompilationDatabase_getAllCompileCommands(db);
    const unsigned int sz = clang_CompileCommands_getSize(cmds);
    auto &ref = data.compileCommands[fileId];
    ref.environment = environment;
    ref.lastModifiedMs = compileCommands.lastModifiedMs();
    for (unsigned int i = 0; i < sz; ++i) {
        CXCompileCommand cmd = clang_CompileCommands_getCommand(cmds, i);
        String args;
        CXString str = clang_CompileCommand_getDirectory(cmd);
        Path compileDir = clang_getCString(str);
        if (!compileDir.isAbsolute() || !compileDir.exists()) {
            bool resolveOk = false;
            debug() << "compileDir doesn't exist: " << compileDir;
            Path resolvedCompileDir = compileDir.resolved(Path::MakeAbsolute, data.project, &resolveOk);
            if (resolveOk) {
                compileDir = resolvedCompileDir;
                debug() << "    resolved to: " << compileDir;
            }
        }
        clang_disposeString(str);
        const unsigned int num = clang_CompileCommand_getNumArgs(cmd);
        for (unsigned int j = 0; j < num; ++j) {
            str = clang_CompileCommand_getArg(cmd, j);
            const char *ch = clang_getCString(str);
            if (strchr(ch, ' ')) {
                args += '"';
                args += ch;
                args += '"';
            } else {
                args += ch;
            }
            clang_disposeString(str);
            if (j < num - 1)
                args += ' ';
        }
        ret = parse(data, std::move(args), compileDir.ensureTrailingSlash(), fileId, cache) || ret;
    }
    clang_CompileCommands_dispose(cmds);
    clang_CompilationDatabase_dispose(db);
    if (!ret) {
        data.compileCommands.remove(fileId);
    }
    return ret;
}

bool Server::parse(IndexParseData &data, String &&arguments, const Path &pwd, uint32_t compileCommandsFileId, SourceCache *cache) const
{
    if (Sandbox::hasRoot() && !data.project.isEmpty() && !data.project.startsWith(Sandbox::root())) {
        error("Invalid --project-root '%s', must be inside --sandbox-root '%s'",
              data.project.constData(), Sandbox::root().constData());
        return false;
    }

    assert(pwd.endsWith('/'));
    List<Path> unresolvedPaths;
    if (!mOptions.argTransform.isEmpty()) {
        Process process;
        if (process.exec(mOptions.argTransform, List<String>() << arguments) == Process::Done) {
            if (process.returnCode() != 0) {
                warning() << "--arg-transform returned" << process.returnCode() << "for" << arguments;
                return false;
            }
            String stdOut = process.readAllStdOut();
            if (!stdOut.isEmpty() && stdOut != arguments) {
                warning() << "Changed\n" << arguments << "\nto\n" << stdOut;
                arguments = std::move(stdOut);
            }
        }
    }

    assert(!compileCommandsFileId || data.compileCommands.contains(compileCommandsFileId));
    const auto &env = compileCommandsFileId ? data.compileCommands[compileCommandsFileId].environment : data.environment;
    SourceList sources = Source::parse(arguments, pwd, env, &unresolvedPaths, cache);
    bool ret = (sources.isEmpty() && unresolvedPaths.size() == 1 && unresolvedPaths.front() == "-");
    debug() << "Got" << sources.size() << "sources, and" << unresolvedPaths << "from" << arguments;
    size_t idx = 0;
    for (Source &source : sources) {
        const Path path = source.sourceFile();

        std::shared_ptr<Project> current = currentProject();
        if (data.project.isEmpty()) {
            const Path unresolvedPath = unresolvedPaths.at(idx++);
            if (current && (current->match(unresolvedPath) || (path != unresolvedPath && current->match(path)))) {
                data.project = current->path();
            } else {
                for (const auto &proj : mProjects) {
                    if (proj.second->match(unresolvedPath) || (path != unresolvedPath && proj.second->match(path))) {
                        data.project = proj.first;
                        break;
                    }
                }
            }

            if (data.project.isEmpty()) {
                data.project = RTags::findProjectRoot(unresolvedPath, RTags::SourceRoot, cache);
                if (data.project.isEmpty() && path != unresolvedPath) {
                    data.project = RTags::findProjectRoot(path, RTags::SourceRoot, cache);
                }
            }
            data.project.resolve(Path::RealPath, pwd);
        }

        if (shouldIndex(source, data.project)) {
            Sources &s = compileCommandsFileId ? data.compileCommands[compileCommandsFileId].sources : data.sources;
            source.compileCommandsFileId = compileCommandsFileId;
            auto &list = s[source.fileId];
            if (!list.contains(source))
                list.append(source);
            ret = true;
        } else {
            debug() << "Shouldn't index" << source;
        }
    }
    return ret;
}

void Server::handleIndexMessage(const std::shared_ptr<IndexMessage> &message, const std::shared_ptr<Connection> &conn)
{
    const Path path = message->compileCommands();
    IndexParseData data;
    data.project = message->projectRoot();
    bool ret = true;
    if (!path.isEmpty()) {
        SourceCache cache;
        if (loadCompileCommands(data, path, message->environment(), &cache)) {
            if (conn)
                conn->write("[Server] Compilation database loading...");
        } else if (conn) {
            ret = false;
            conn->write("[Server] Compilation failed to load.");
        }
    } else {
        data.environment = std::move(message->takeEnvironment());
        String arguments = std::move(message->takeArguments());
        if (message->flags() & IndexMessage::GuessFlags) {
            arguments = guessArguments(arguments, message->workingDirectory(), data.project);
            if (arguments.isEmpty()) {
                conn->write("Can't guess args from arguments");
                ret = false;
            }
        }
        if (ret)
            ret = parse(data, std::move(arguments), message->workingDirectory());
    }
    if (conn)
        conn->finish(ret ? 0 : 1);
    if (ret) {
        auto proj = addProject(data.project.ensureTrailingSlash());
        if (proj) {
            assert(proj);
            proj->processParseData(std::move(data));
            if (!currentProject())
                setCurrentProject(proj);
        }
    }
}

void Server::handleLogOutputMessage(const std::shared_ptr<LogOutputMessage> &message, const std::shared_ptr<Connection> &conn)
{
    auto log = std::make_shared<RTagsLogOutput>(message->level(), message->flags(), conn);
    log->add();
}

void Server::handleIndexDataMessage(const std::shared_ptr<IndexDataMessage> &message, const std::shared_ptr<Connection> &conn)
{
    debug() << "Got handleIndexDataMessage";
    mJobScheduler->handleIndexDataMessage(message);
    conn->finish();
    mIndexDataMessageReceived();
}

void Server::handleQueryMessage(const std::shared_ptr<QueryMessage> &message, const std::shared_ptr<Connection> &conn)
{
    Log(message->flags() & QueryMessage::SilentQuery ? LogLevel::Warning : LogLevel::Error,
        LogOutput::StdOut|LogOutput::TrailingNewLine) << message->commandLine();
    conn->setSilent(message->flags() & QueryMessage::Silent);

    switch (message->type()) {
    case QueryMessage::Invalid:
        assert(0);
        break;
    case QueryMessage::Sources:
        sources(message, conn);
        break;
    case QueryMessage::IncludeFile:
        includeFile(message, conn);
        break;
    case QueryMessage::GenerateTest:
        generateTest(message, conn);
        break;
    case QueryMessage::DumpCompletions:
        dumpCompletions(message, conn);
        break;
    case QueryMessage::DumpCompileCommands:
        dumpCompileCommands(message, conn);
        break;
    case QueryMessage::SendDiagnostics:
        sendDiagnostics(message, conn);
        break;
    case QueryMessage::DeadFunctions:
        deadFunctions(message, conn);
        break;
    case QueryMessage::CodeCompleteAt:
        codeCompleteAt(message, conn);
        break;
    case QueryMessage::Suspend:
        suspend(message, conn);
        break;
    case QueryMessage::IsIndexing:
        isIndexing(message, conn);
        break;
    case QueryMessage::LastIndexed:
        lastIndexed(message, conn);
        break;
    case QueryMessage::RemoveFile:
        removeFile(message, conn);
        break;
    case QueryMessage::JobCount:
        jobCount(message, conn);
        break;
    case QueryMessage::FixIts:
        fixIts(message, conn);
        break;
    case QueryMessage::FindFile:
        findFile(message, conn);
        break;
    case QueryMessage::DumpFile:
        startClangThread(message, conn);
        break;
    case QueryMessage::Validate:
        validate(message, conn);
        break;
    case QueryMessage::DumpFileMaps:
        dumpFileMaps(message, conn);
        break;
    case QueryMessage::Diagnose:
        diagnose(message, conn);
        break;
    case QueryMessage::Dependencies:
        dependencies(message, conn);
        break;
    case QueryMessage::DeleteProject:
        removeProject(message, conn);
        break;
    case QueryMessage::Project:
        project(message, conn);
        break;
    case QueryMessage::Reindex:
    case QueryMessage::CheckReindex:
        reindex(message, conn);
        break;
    case QueryMessage::ClearProjects:
        clearProjects(message, conn);
        break;
    case QueryMessage::SymbolInfo:
        symbolInfo(message, conn);
        break;
    case QueryMessage::FollowLocation:
        followLocation(message, conn);
        break;
    case QueryMessage::ReferencesLocation:
        referencesForLocation(message, conn);
        break;
    case QueryMessage::ReferencesName:
        referencesForName(message, conn);
        break;
    case QueryMessage::ListSymbols:
        listSymbols(message, conn);
        break;
    case QueryMessage::FindSymbols:
        findSymbols(message, conn);
        break;
    case QueryMessage::Status:
        status(message, conn);
        break;
    case QueryMessage::IsIndexed:
        isIndexed(message, conn);
        break;
    case QueryMessage::HasFileManager:
        hasFileManager(message, conn);
        break;
    case QueryMessage::PreprocessFile:
    case QueryMessage::AsmFile:
        preprocessFile(message, conn);
        break;
    case QueryMessage::ReloadFileManager:
        reloadFileManager(message, conn);
        break;
    case QueryMessage::SetBuffers:
        setBuffers(message, conn);
        break;
    case QueryMessage::ClassHierarchy:
        classHierarchy(message, conn);
        break;
    case QueryMessage::DebugLocations:
        debugLocations(message, conn);
        break;
    case QueryMessage::Tokens:
        tokens(message, conn);
        break;
    case QueryMessage::IncludePath:
        includePath(message, conn);
        break;
    }
}

void Server::followLocation(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    const Location loc = query->location();
    if (loc.isNull()) {
        conn->write("Not indexed");
        conn->finish(RTags::NotIndexed);
        return;
    }
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project) {
        error("No project");
        conn->write("Not indexed");
        conn->finish(RTags::NotIndexed);
        return;
    }

    prepareCompletion(query, loc.fileId(), project);

    {
        FollowLocationJob job(loc, query, project);
        if (!job.run(conn)) {
            conn->finish();
            return;
        }
    }

    /* We will try with another project under the following circumstances:

       - We didn't find anything with the current project
       - The path in question (likely a header) does not start with the current
       project's path (there's room for mistakes here with symlinks).
       - The file in question does start with another project's path
    */

    const Path path = loc.path();
    if (!path.startsWith(project->path())) {
        for (const auto &proj : mProjects) {
            if (proj.second != project) {
                Path paths[] = { proj.first, proj.first };
                paths[1].resolve();
                for (const Path &projectPath : paths) {
                    if (path.startsWith(projectPath)) {
                        FollowLocationJob job(loc, query, proj.second);
                        if (job.run(conn)) {
                            conn->finish();
                            return;
                        }
                    }
                }
            }
        }
    }
    if (!project->dependencies().contains(loc.fileId())) {
        conn->write("Not indexed");
        conn->finish(RTags::NotIndexed);
    } else {
        conn->finish(RTags::GeneralFailure);
    }
}

void Server::lastIndexed(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    // Path path = query->path();
    const Match match = query->match();
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project)
        project = currentProject();

    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    conn->write<128>("%ld", project->lastIdleTime());
    conn->finish();
}

void Server::isIndexing(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    std::shared_ptr<Project> project = projectForQuery(query);
    if (project) {
        if (project->isIndexing()) {
            conn->write("1");
            conn->finish();
            return;
        }
    } else {
        for (const auto &it : mProjects) {
            if (it.second->isIndexing()) {
                conn->write("1");
                conn->finish();
                return;
            }
        }
    }
    conn->write("0");
    conn->finish();
}

void Server::removeFile(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    // Path path = query->path();
    const Match match = query->match();
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project)
        project = currentProject();

    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    const int count = project->remove(match);
    // error() << count << query->query();
    if (count) {
        conn->write<128>("Removed %d files", count);
    } else {
        conn->write("No matches");
    }
    conn->finish();
}

void Server::findFile(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    std::shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    FindFileJob job(query, project);
    const int ret = job.run(conn);
    conn->finish(ret);
}

void Server::startClangThread(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    const uint32_t fileId = Location::fileId(query->query());
    if (!fileId) {
        conn->write<256>("%s is not indexed", query->query().constData());
        conn->finish();
        return;
    }

    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project) {
        conn->write<256>("%s is not indexed", query->query().constData());
        conn->finish();
        return;
    }

    if (!project->dependencies().contains(fileId)) {
        conn->write("Not indexed");
        conn->finish(RTags::NotIndexed);
        return;
    }

    const Source source = project->source(fileId, query->buildIndex());
    if (!source.isNull()) {
        ClangThread *thread = new ClangThread(query, source, conn);
        thread->start(Thread::Normal, 8 * 1024 * 1024); // 8MiB stack size
    } else {
        conn->write<256>("%s build: %d not found", query->query().constData(), query->buildIndex());
        conn->finish();
    }
}

void Server::dumpFileMaps(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    Path path;
    Deserializer deserializer(query->query());
    deserializer >> path;
    const uint32_t fileId = Location::fileId(path);
    if (!fileId) {
        conn->write<256>("%s is not indexed", query->query().constData());
        conn->finish();
        return;
    }

    std::shared_ptr<Project> project;
    if (currentProject() && currentProject()->isIndexed(fileId)) {
        project = currentProject();
    } else {
        for (const auto &p : mProjects) {
            if (p.second->isIndexed(fileId)) {
                project = p.second;
                setCurrentProject(project);
                break;
            }
        }
    }
    if (!project) {
        conn->write<256>("%s is not indexed", query->query().constData());
        conn->finish();
        return;
    }

    project->dumpFileMaps(query, conn);
    conn->finish();
}

void Server::diagnose(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    uint32_t fileId = 0;
    if (!query->query().isEmpty()) {
        fileId = Location::fileId(query->query());
        if (!fileId) {
            conn->write<256>("%s is not indexed", query->query().constData());
            conn->finish();
            return;
        }
    }

    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project) {
        if (!fileId)
            project = mCurrentProject.lock();
        if (!project) {
            conn->write<256>("%s is not indexed", query->query().constData());
            conn->finish();
            return;
        }
    }

    if (fileId)
        prepareCompletion(query, fileId, project);

    if (query->flags() & QueryMessage::SynchronousDiagnostics) {
        conn->write(project->diagnosticsToString(query->flags(), fileId));
    } else {
        project->diagnose(fileId);
    }
    conn->finish();
}

void Server::generateTest(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    auto formatLocation = [](Location location) {
        return String::format<1024>("{0}/%s:%d:%d:",
                                    location.path().fileName(),
                                    location.line(),
                                    location.column());
    };
    const uint32_t fileId = Location::fileId(query->query());
    if (!fileId) {
        conn->write<256>("%s is not indexed", query->query().constData());
        conn->finish();
        return;
    }

    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project) {
        conn->write<256>("%s is not indexed", query->query().constData());
        conn->finish();
        return;
    }
    Project::FileMapScopeScope scope(project);

    const Source source = project->source(fileId, query->buildIndex());
    if (!source.isNull()) {
        const Flags<Source::CommandLineFlag> flags = (Source::Default
                                                      | Source::ExcludeDefaultDefines
                                                      | Source::ExcludeDefaultIncludePaths
                                                      | Source::ExcludeDefaultArguments);

        const Path root = source.sourceFile().parentDir().ensureTrailingSlash();
        List<String> compile = source.toCommandLine(flags);
        for (auto &ref : compile) {
            const int idx = ref.indexOf(root);
            if (idx != -1)
                ref.remove(idx, root.size());
        }
        compile.append(source.sourceFile().fileName());

        Value tests;

        for (const auto &dep : project->dependencies()) {
            auto symbols = project->openSymbols(dep.first);
            if (!symbols)
                continue;
            const int count = symbols->count();
            for (int i=0; i<count; ++i) {
                const Location loc = symbols->keyAt(i);
                const Symbol target = project->findTarget(loc);
                if (!target.isNull()) {
                    Map<String, Value> test;
                    test["name"] = "follow_symbol";
                    List<String> rcCommand;
                    rcCommand << "--follow-location" << formatLocation(loc);
                    test["rc-command"] = rcCommand;
                    List<String> expectation;
                    expectation << formatLocation(target.location);
                    test["expectation"] = expectation;
                    tests.push_back(test);
                }
            }
        }
        if (tests.isNull()) {
            conn->finish("No tests generated");
        } else {
            conn->finish(tests.toJSON(true));
        }
    } else {
        conn->write<256>("%s build: %d not found", query->query().constData(), query->buildIndex());
        conn->finish();
    }
}

void Server::symbolInfo(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    const String data = query->query();
    Deserializer deserializer(data);
    Path path;
    uint32_t line, column, line2, column2;
    Set<String> kinds; // This is serialized as a of List<String>
    deserializer >> path >> line >> column >> line2 >> column2 >> kinds;
    uint32_t fileId = Location::fileId(path);
    if (!fileId) {
        path.resolve();
        fileId = Location::fileId(path);
        if (!fileId) {
            conn->write("Not indexed");
            conn->finish(RTags::NotIndexed);
            return;
        }
    }

    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project) {
        List<Match> matches;
        matches << path;
        project = projectForMatches(matches);
    }
    if (!project || !project->dependencies().contains(fileId)) {
        conn->write("Not indexed");
        conn->finish(RTags::NotIndexed);
        return;
    }

    prepareCompletion(query, fileId, project);

    const Location start(fileId, line, column);
    const Location end = line2 ? Location(fileId, line2, column2) : Location();

    SymbolInfoJob job(start, end, std::move(kinds), query, project);
    const int ret = job.run(conn);
    conn->finish(ret);
}

void Server::includePath(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    const Location loc = query->location();
    if (loc.isNull()) {
        conn->write("Not indexed");
        conn->finish(RTags::NotIndexed);
        return;
    }

    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project) {
        error("No project");
        conn->write("Not indexed");
        conn->finish(RTags::NotIndexed);
        return;
    }

    prepareCompletion(query, loc.fileId(), project);

    {
        IncludePathJob job(loc, query, project);
        if (!job.run(conn)) {
            conn->finish();
            return;
        }
    }
}

void Server::dependencies(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    Path path;
    Deserializer deserializer(query->query());
    deserializer >> path;
    const uint32_t fileId = Location::fileId(path);
    if (!fileId && !path.isEmpty()) {
        conn->write<256>("%s is not indexed", query->query().constData());
        conn->finish();
        return;
    }

    std::shared_ptr<Project> project;
    if (fileId) {
        if (currentProject() && currentProject()->isIndexed(fileId)) {
            project = currentProject();
        } else {
            for (const auto &p : mProjects) {
                if (p.second->isIndexed(fileId)) {
                    project = p.second;
                    setCurrentProject(project);
                    break;
                }
            }
        }
    } else {
        project = currentProject();
    }
    if (!project) {
        conn->write<256>("%s is not indexed", query->query().constData());
        conn->finish();
        return;
    }

    DependenciesJob job(query, project);
    const int ret = job.run(conn);
    conn->finish(ret);
}

void Server::fixIts(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    std::shared_ptr<Project> project = projectForQuery(query);
    String out;
    if (project) {
        uint32_t fileId = Location::fileId(query->query());
        if (fileId) {
            prepareCompletion(query, fileId, project);
            out = project->fixIts(fileId);
            if (!out.isEmpty())
                conn->write(out);
        }
    }
    conn->finish();
}

void Server::referencesForLocation(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    const Location loc = query->location();
    if (loc.isNull()) {
        conn->write("Not indexed");
        conn->finish(RTags::NotIndexed);
        return;
    }
    std::shared_ptr<Project> project = projectForQuery(query);

    if (!project) {
        error("No project");
        conn->write("Not indexed");
        conn->finish(RTags::NotIndexed);
        return;
    }

    prepareCompletion(query, loc.fileId(), project);

    if (!project->dependencies().contains(loc.fileId())) {
        conn->write("Not indexed");
        conn->finish(RTags::NotIndexed);
        return;
    }

    ReferencesJob job(loc, query, project);
    const int ret = job.run(conn);
    conn->finish(ret);
}

void Server::referencesForName(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    const String name = query->query();

    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project)
        project = currentProject();

    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    ReferencesJob job(name, query, project);
    const int ret = job.run(conn);
    conn->finish(ret);
}

void Server::findSymbols(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    const String partial = query->query();

    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project)
        project = currentProject();

    int ret = 0;
    if (!project) {
        ret = 1;
        error("No project");
    } else {
        FindSymbolsJob job(query, project);
        ret = job.run(conn);
    }
    conn->finish(ret);
}

void Server::listSymbols(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    const String partial = query->query();

    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project)
        project = currentProject();

    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    ListSymbolsJob job(query, project);
    const int ret = job.run(conn);
    conn->finish(ret);
}

void Server::status(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    StatusJob job(query, currentProject());
    const int ret = job.run(conn);
    conn->finish(ret);
}

void Server::isIndexed(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    String ret = "unknown";
    const Match match = query->match();
    std::shared_ptr<Project> project = projectForQuery(query);
    if (project) {
        bool indexed = false;
        if (project->match(match, &indexed)) {
            if (indexed) {
                project->prepare(match.fileId());
                prepareCompletion(query, match.fileId(), project);
            }
            ret = indexed ? "indexed" : "managed";
        }
    }

    if (!(query->flags() & QueryMessage::SilentQuery))
        warning("=> %s", ret.constData());
    conn->write(ret);
    conn->finish();
}

void Server::reloadFileManager(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project)
        project = currentProject();

    if (project) {
        if (mOptions.options & NoFileManager) {
            conn->write<512>("Not watching files");
            conn->finish(RTags::GeneralFailure);
        } else {
            conn->write<512>("Reloading files for %s", project->path().constData());
            conn->finish();
            project->fileManager()->load(FileManager::Asynchronous);
        }
    } else {
        conn->write("No current project");
        conn->finish();
    }
}

void Server::hasFileManager(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    const Path path = query->query();
    std::shared_ptr<Project> project = projectForQuery(query);
    if (project && (project->fileManager() ? project->fileManager()->contains(path) : project->match(query->match()))) {
        if (!(query->flags() & QueryMessage::SilentQuery))
            warning("=> 1");
        conn->write("1");
    } else {
        if (!(query->flags() & QueryMessage::SilentQuery))
            warning("=> 0");
        conn->write("0");
    }
    conn->finish();
}

void Server::includeFile(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project) {
        conn->write("No project");
        conn->finish();
        return;
    }

    IncludeFileJob job(query, project);
    conn->finish(job.run(conn));
}

void Server::preprocessFile(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project) {
        conn->write("No project");
        conn->finish();
        return;
    }

    Path path = query->query();
    uint32_t fileId = Location::fileId(path);
    if (!fileId) {
        path.resolve();
        fileId = Location::fileId(path);
    }
    if (fileId)
        prepareCompletion(query, fileId, project);
    const Source source = project->source(fileId, query->buildIndex());
    if (!source.isValid()) {
        conn->write<256>("%s build: %d not found", query->query().constData(), query->buildIndex());
        conn->finish();
    } else {
        Preprocessor *pre = new Preprocessor((query->type() == QueryMessage::PreprocessFile) ? Preprocessor::Preprocess : Preprocessor::Asm, source, conn);
        pre->preprocess();
    }
}

void Server::clearProjects(ClearMode mode)
{
    Path::rmdir(mOptions.dataDir);
    setCurrentProject(std::shared_ptr<Project>());
    for (auto p : mProjects) {
        p.second->destroy();
    }
    mProjects.clear();
    if (mode == Clear_All)
        Location::init(Hash<Path, uint32_t>());
}

void Server::reindex(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    Match match = query->match();
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project) {
        project = currentProject();
        if (!project) {
            error("No project");
            conn->finish();
            return;
        }
    }

    std::shared_ptr<Connection> wait;
    if (query->flags() & QueryMessage::Wait)
        wait = conn;

    const int count = project->reindex(match, query, wait);
    // error() << count << query->query();
    if (count) {
        conn->write<128>("Dirtied %d files", count);
    } else {
        conn->write("No matches");
    }
    if (!wait)
        conn->finish();
}

bool Server::shouldIndex(const Source &source, const Path &srcRoot) const
{
    if (srcRoot.isEmpty()) {
        warning() << "Shouldn't index" << source.sourceFile() << "because of missing srcRoot";
        return false;
    }
    assert(source.isIndexable());
    if (mOptions.ignoredCompilers.contains(source.compiler())) {
        warning() << "Shouldn't index" << source.sourceFile() << "because of ignored compiler";
        return false;
    }

    const Path sourceFile = source.sourceFile();

    if (Filter::filter(sourceFile, mOptions.excludeFilters) == Filter::Filtered) {
        error() << "Shouldn't index" << source.sourceFile() << "because of exclude filter";
        return false;
    }

    if (Sandbox::hasRoot() && !srcRoot.isEmpty() && !srcRoot.startsWith(Sandbox::root())) {
        error("Invalid project root for %s '%s', must be inside --sandbox-root '%s'",
              sourceFile.constData(), srcRoot.constData(), Sandbox::root().constData());
        return false;
    }

    return true;
}

void Server::setCurrentProject(const std::shared_ptr<Project> &project)
{
    std::shared_ptr<Project> old = currentProject();
    if (project != old) {
        if (old && old->fileManager())
            old->fileManager()->clearFileSystemWatcher();
        mCurrentProject = project;
        if (project) {
            Path::mkdir(mOptions.dataDir);
            FILE *f = fopen((mOptions.dataDir + ".currentProject").constData(), "w");
            if (f) {
                Path p = project->path();
                RTags::encodePath(p);
                if (!fwrite(p.constData(), p.size(), 1, f) || !fwrite("\n", 1, 1, f)) {
                    error() << "error writing to" << (mOptions.dataDir + ".currentProject");
                    fclose(f);
                    unlink((mOptions.dataDir + ".currentProject").constData());
                } else {
                    fclose(f);
                }
            } else {
                error() << "error opening" << (mOptions.dataDir + ".currentProject") << "for write";
            }
            if (!(mOptions.options & NoFileManager))
                project->fileManager()->load(FileManager::Asynchronous);
            mJobScheduler->sort();
            project->check(Project::Check_Explicit);
            // project->diagnoseAll();
        } else {
            Path::rm(mOptions.dataDir + ".currentProject");
        }
    }
}

std::shared_ptr<Project> Server::projectForQuery(const std::shared_ptr<QueryMessage> &query)
{
    List<Match> matches;
    if (query->flags() & QueryMessage::HasLocation) {
        matches << query->location().path();
    } else if (query->flags() & QueryMessage::HasMatch) {
        matches << query->match();
    }
    if (!query->currentFile().isEmpty())
        matches << query->currentFile();

    return projectForMatches(matches);
}

std::shared_ptr<Project> Server::projectForMatches(const List<Match> &matches)
{
    std::shared_ptr<Project> cur = currentProject();
    // give current a chance first to avoid switching project when using system headers etc
    for (const Match &match : matches) {
        if (cur && cur->match(match))
            return cur;

        for (const auto &it : mProjects) {
            if (it.second != cur && it.second->match(match)) {
                setCurrentProject(it.second);
                return it.second;
            }
        }
    }
    return std::shared_ptr<Project>();
}

void Server::removeProject(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    const Match match = query->match();
    auto it = mProjects.begin();
    bool found = false;
    while (it != mProjects.end()) {
        auto cur = it++;
        if (cur->second->match(match)) {
            found = true;
            if (currentProject() == cur->second) {
                setCurrentProject(std::shared_ptr<Project>());
            }
            Path path = cur->first;
            conn->write<128>("Deleted project: %s", path.constData());
            RTags::encodePath(path);
            Path::rmdir(mOptions.dataDir + path);
            warning() << "Deleted" << (mOptions.dataDir + path);
            cur->second->destroy();
            mProjects.erase(cur);
        }
    }
    if (!found) {
        conn->write<128>("No projects matching %s", match.pattern().constData());
    }
    conn->finish();
}

void Server::project(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    std::shared_ptr<Project> project = projectForQuery(query);
    if (project) {
        setCurrentProject(project);
    }

    if (project || query->flags() & QueryMessage::CurrentProjectOnly) {
        if (std::shared_ptr<Project> current = currentProject()) {
            conn->write(current->path());
        }
    } else if (query->query().isEmpty()) {
        const std::shared_ptr<Project> current = currentProject();
        for (const auto &it : mProjects) {
            conn->write<128>("%s%s", it.first.constData(), it.second == current ? " <=" : "");
        }
    } else {
        std::shared_ptr<Project> selected;
        bool error = false;
        const Match match = query->match();
        const auto it = mProjects.find(match.pattern());
        bool ok = false;
        unsigned long long index = query->query().toULongLong(&ok);
        if (it != mProjects.end()) {
            selected = it->second;
        } else {
            if (ok) {
                if (index < mProjects.size()) {
                    selected = std::next(mProjects.cbegin(), index)->second;
                    assert(selected);
                }
            }
            if (!selected) {
                for (const auto &pit : mProjects) {
                    assert(pit.second);
                    if (pit.second->match(match)) {
                        if (error) {
                            conn->write(pit.first);
                        } else if (selected) {
                            error = true;
                            conn->write<128>("Multiple matches for %s", match.pattern().constData());
                            conn->write(selected->path());
                            conn->write(pit.first);
                            selected.reset();
                        } else {
                            selected = pit.second;
                        }
                    }
                }
            }
        }
        if (selected) {
            if (selected == currentProject()) {
                conn->write<128>("%s is already the active project", selected->path().constData());
            } else {
                setCurrentProject(selected);
                conn->write<128>("Selected project: %s for %s",
                                 selected->path().constData(),
                                 match.pattern().constData());
            }
        } else if (!error) {
            conn->write<128>("No matches for %s", match.pattern().constData());
        }
    }
    conn->finish();
}

void Server::jobCount(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    enum { MaxJobCount = 512 };
    String q = query->query();
    if (q.isEmpty()) {
        conn->write<128>("Running with %zu jobs", mOptions.jobCount);
    } else {
        int jobCount = -1;
        bool ok = false;
        if (q == "default") {
            ok = true;
            jobCount = mDefaultJobCount;
        } else if (q == "pop") {
            if (mJobCountStack.isEmpty()) {
                conn->write<128>("Job count stack is empty");
            } else {
                jobCount = mJobCountStack.back();
                mJobCountStack.pop_back();
                ok = true;
            }
        } else if (q.startsWith("push:")) {
            jobCount = q.mid(5).toLongLong(&ok);
            if (ok && jobCount > 0 && jobCount < MaxJobCount) {
                mJobCountStack.append(mOptions.jobCount);
            };
        } else {
            jobCount = q.toLongLong(&ok);
        }
        if (!ok || jobCount < 0 || jobCount > MaxJobCount) {
            conn->write<128>("Invalid job count %s (%d)", query->query().constData(), jobCount);
        } else {
            mOptions.jobCount = jobCount;
            conn->write<128>("Changed jobs to %zu", mOptions.jobCount);
            mJobScheduler->startJobs();
        }
    }
    conn->finish();
}

void Server::deadFunctions(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project)
        project = currentProject();
    if (project) {
        class DeadFunctionsJob : public QueryJob
        {
        public:
            DeadFunctionsJob(const std::shared_ptr<QueryMessage> &msg, const std::shared_ptr<Project> &project)
                : QueryJob(msg, project)
            {}
            virtual int execute() override
            {
                const uint32_t fileId = Location::fileId(queryMessage()->query());
                if (fileId)
                    Server::instance()->prepareCompletion(queryMessage(), fileId, project());
                bool raw = false;
                if (!(queryFlags() & (QueryMessage::JSON|QueryMessage::Elisp))) {
                    raw = true;
                    setPieceFilters(std::move(Set<String>() << "location"));
                }
                bool failed = false;
                const std::shared_ptr<Project> proj = project();
                auto process = [this, proj, &failed](uint32_t file) {
                    for (const auto &pair : proj->findDeadFunctions(file)) {
                        String out = symbolToString(pair.first);
                        if (!out.isEmpty()) {
                            out.chop(1);
                            out += String::format<32>(" - %zu callers\n", pair.second);
                            if (!write(out, Unfiltered)) {
                                failed = true;
                                break;
                            }
                        }
                    }
                };
                if (!fileId) {
                    Set<uint32_t> all = proj->dependencies(0, Project::All);
                    all.remove([](uint32_t file) { return Location::path(file).isSystem(); });
                    size_t idx = 0;
                    const Path projectPath = proj->path();
                    for (uint32_t file : all) {
                        if (raw) {
                            Path p = Location::path(file);
                            const char *ch = p.constData();
                            if (!(queryFlags() & QueryMessage::AbsolutePath) && p.startsWith(projectPath))
                                ch += projectPath.size();
                            if (!write(String::format<256>("%zu/%zu %s", ++idx, all.size(), ch))) {
                                failed = true;
                                break;
                            }
                        }
                        process(file);
                        if (failed)
                            break;
                    }
                } else {
                    process(fileId);
                }
                return 0;
            }
        } job(query, project);
        job.run(conn);
    }
    conn->finish();
}

void Server::sendDiagnostics(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    if (testLog(RTags::DiagnosticsLevel))
        logDirect(RTags::DiagnosticsLevel, query->query());
    conn->finish();
}

void Server::clearProjects(const std::shared_ptr<QueryMessage> &/*query*/, const std::shared_ptr<Connection> &conn)
{
    clearProjects(Clear_All);
    conn->write("Cleared projects");
    conn->finish();
}

void Server::sources(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    const Path path = query->query();
    const bool flagsOnly = query->flags() & QueryMessage::CompilationFlagsOnly;
    const bool splitLine = query->flags() & QueryMessage::CompilationFlagsSplitLine;
    const bool pwd = query->flags() & QueryMessage::CompilationFlagsPwd;
    auto format = [flagsOnly, splitLine, pwd](const Source &source) {
        String ret;
        if (pwd)
            ret += "pwd: " + source.directory + "\n";
        if (flagsOnly) {
            const Flags<Source::CommandLineFlag> flags = (Source::Default
                                                          |Source::ExcludeDefaultArguments
                                                          |Source::IncludeCompiler
                                                          |Source::IncludeSourceFile
                                                          |Source::ExcludeDefaultDefines
                                                          |Source::ExcludeDefaultIncludePaths);
            ret += String::join(source.toCommandLine(flags), splitLine ? '\n' : ' ');
        } else if (splitLine) {
            ret += String::join(source.toString().split(' '), '\n');
        } else {
            ret += source.toString();
        }
        return ret;
    };
    if (path.isFile()) {
        std::shared_ptr<Project> project = projectForQuery(query);
        if (project) {
            const uint32_t fileId = Location::fileId(path);
            if (fileId) {
                prepareCompletion(query, fileId, project);
                SourceList sources = project->sources(fileId);
                if (sources.isEmpty() && path.isHeader()) {
                    Set<uint32_t> seen;
                    std::function<uint32_t(uint32_t)> findSourceFileId = [&findSourceFileId, &project, &seen](uint32_t file) {
                        DependencyNode *node = project->dependencyNode(file);
                        uint32_t ret = 0;
                        if (node) {
                            for (const auto &dep : node->dependents) {
                                if (!seen.insert(dep.first))
                                    continue;

                                if (Location::path(dep.first).isSource()) {
                                    ret = dep.first;
                                    break;
                                } else {
                                    ret  = findSourceFileId(dep.first);
                                    if (ret)
                                        break;
                                }
                            }
                        }
                        return ret;
                    };

                    const uint32_t source = findSourceFileId(fileId);
                    sources = project->sources(source);
                    if (sources.size() > 1) {
                        auto it = sources.begin();
                        ++it;
                        while (it != sources.end()) {
                            sources.erase(it++);
                        }
                    }
                }
                int idx = 0;
                for (const auto &src : sources) {
                    String out;
                    if (sources.size() > 1)
                        out = String::format<4>("%d: ", idx);
                    conn->write(format(src));
                }
            }
            conn->finish();
            return;
        }
    }

    if (std::shared_ptr<Project> project = currentProject()) {
        if (query->flags() & (QueryMessage::CompilationFlagsOnly|QueryMessage::CompilationFlagsSplitLine|QueryMessage::CompilationFlagsPwd)) {
            project->forEachSource([&conn, &format](const Source &source) { return conn->write(format(source)) ? Project::Continue : Project::Stop; });
        } else {
            project->indexParseData().write([&conn](const String &str) { return conn->write(str); });
        }
    } else {
        conn->write("No project");
    }
    conn->finish();
}

void Server::dumpCompletions(const std::shared_ptr<QueryMessage> &/*query*/, const std::shared_ptr<Connection> &conn)
{
    if (mCompletionThread) {
        conn->write(mCompletionThread->dump());
    } else {
        conn->write("No completions");
    }
    conn->finish();
}

void Server::dumpCompileCommands(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project)
        project = currentProject();
    if (!project) {
        conn->write("No current project");
        conn->finish(RTags::GeneralFailure);
        return;
    }

    conn->write(project->toCompileCommands());
    conn->finish();
}

void Server::suspend(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    Path p;
    String modeString;
    const String pattern = query->match().pattern();
    Deserializer deserializer(pattern);
    deserializer >> p >> modeString;
    enum Mode {
        ListFiles,
        All,
        Clear,
        FileToggle,
        FileOn,
        FileOff
    } mode = ListFiles;
    if (p == "clear") {
        mode = Clear;
        assert(modeString.isEmpty());
    } else if (p == "all") {
        mode = All;
        assert(modeString.isEmpty());
    } else if (!p.isEmpty()) {
        if (modeString == "on") {
            mode = FileOn;
        } else if (modeString == "off") {
            mode = FileOff;
        } else {
            mode = FileToggle;
        }
    }

    List<Match> matches;
    if (!query->currentFile().isEmpty())
        matches.push_back(query->currentFile());
    if (mode == FileOn || mode == FileOff || mode == FileToggle)
        matches.push_back(p);
    std::shared_ptr<Project> project;
    if (matches.isEmpty()) {
        project = currentProject();
    } else {
        project = projectForMatches(matches);
    }

    const bool old = mSuspended;
    switch (mode) {
    case All:
        mSuspended = true;
        conn->write("All files are suspended.");
        break;
    case Clear:
        mSuspended = false;
        if (project)
            project->clearSuspendedFiles();
        conn->write("No files suspended.");
        break;
    case ListFiles:
        if (mSuspended)
            conn->write("All files are suspended.");
        if (project) {
            const Set<uint32_t> suspendedFiles = project->suspendedFiles();
            if (suspendedFiles.isEmpty()) {
                conn->write<512>("No files suspended for project %s", project->path().constData());
            } else {
                for (const auto &it : suspendedFiles)
                    conn->write<512>("%s is suspended", Location::path(it).constData());
            }
        }
        break;
    case FileOn:
    case FileOff:
    case FileToggle:
        if (!project) {
            conn->write("No project");
        } else if (!p.isFile()) {
            conn->write<512>("%s doesn't seem to exist", p.constData());
        } else {
            const uint32_t fileId = Location::fileId(p);
            if (fileId) {
                bool suspended = false;
                switch (mode) {
                case FileToggle: suspended = project->toggleSuspendFile(fileId); break;
                case FileOff: suspended = false; project->setSuspended(fileId, false); break;
                case FileOn: suspended = true; project->setSuspended(fileId, true); break;
                default: assert(0);
                }
                conn->write<512>("%s is no%s suspended", p.constData(),
                                 suspended ? "w" : " longer");
            } else {
                conn->write<512>("%s is not indexed", p.constData());
            }
        }
        break;
    }
    conn->finish();

    if (old && !mSuspended)
        mJobScheduler->startJobs();
}

void Server::setBuffers(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    const String encoded = query->query();
    if (encoded.isEmpty()) {
        for (const auto &buffer : mActiveBuffers) {
            conn->write<1024>("%s: %s", Location::path(buffer.first).constData(), buffer.second == Active ? "active" : "open");
        }
    } else {
        mActiveBuffersSet = true;
        Deserializer deserializer(encoded);
        unsigned char version;
        deserializer >> version;
        if (version != '1') {
            conn->write("Mismatched rc and rdm, wrong version");
            conn->finish();
            return;
        }
        int mode;
        deserializer >> mode;
        Hash<Path, bool> paths;
        deserializer >> paths;
        const size_t oldCount = mActiveBuffers.size();
        if (mode == 0 || mode == 1) {
            if (mode == 0)
                mActiveBuffers.clear();
            for (const auto &path : paths) {
                if (uint32_t fileId = Location::insertFile(path.first))
                    mActiveBuffers[fileId] = path.second ? Active : Open;
            }
        } else {
            assert(mode == -1);
            for (const auto &path : paths) {
                mActiveBuffers.remove(Location::insertFile(path.first));
            }
        }
        if (oldCount < mActiveBuffers.size()) {
            conn->write<32>("Added %zu buffers", mActiveBuffers.size() - oldCount);
        } else if (oldCount > mActiveBuffers.size()) {
            conn->write<32>("Removed %zu buffers", oldCount - mActiveBuffers.size());
        } else {
            conn->write<32>("We still have %zu buffers", oldCount);
        }
    }
    mJobScheduler->sort();
    conn->finish();
}

void Server::classHierarchy(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    const Location loc = query->location();
    if (loc.isNull()) {
        conn->write("Not indexed");
        conn->finish(RTags::NotIndexed);
        return;
    }
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project) {
        error("No project");
        conn->write("Not indexed");
        conn->finish(RTags::NotIndexed);
        return;
    }

    if (!project->dependencies().contains(loc.fileId())) {
        conn->write("Not indexed");
        conn->finish(RTags::NotIndexed);
        return;
    }

    ClassHierarchyJob job(loc, query, project);
    conn->finish(job.run(conn));
}

void Server::debugLocations(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    const String str = query->query();
    if (str == "clear" || str == "none") {
        mOptions.debugLocations.clear();
    } else if (str == "all" || str == "*") {
        mOptions.debugLocations.clear();
        mOptions.debugLocations << "all";
    } else if (!str.isEmpty()) {
        mOptions.debugLocations << str;
    }
    if (mOptions.debugLocations.isEmpty()) {
        conn->write("No debug locations");
    } else {
        conn->write<1024>("Debug locations:\n%s", String::join(mOptions.debugLocations, '\n').constData());
    }
    conn->finish();
}


void Server::tokens(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    Path path;
    uint32_t from, to;
    Deserializer deserializer(query->query());
    deserializer >> path >> from >> to;
    const uint32_t fileId = Location::fileId(path);
    if (!fileId) {
        conn->write<256>("%s is not indexed", query->query().constData());
        conn->finish();
        return;
    }

    std::shared_ptr<Project> project;
    if (currentProject() && currentProject()->isIndexed(fileId)) {
        project = currentProject();
    } else {
        for (const auto &p : mProjects) {
            if (p.second->isIndexed(fileId)) {
                project = p.second;
                setCurrentProject(project);
                break;
            }
        }
    }
    if (!project) {
        conn->write<256>("%s is not indexed", query->query().constData());
        conn->finish();
        return;
    }

    TokensJob job(query, fileId, from, to, project);
    conn->finish(job.run(conn));
}

void Server::validate(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project)
        project = mCurrentProject.lock();
    if (!project) {
        error("No project");
        conn->write("No current project");
        conn->finish(RTags::GeneralFailure);
        return;
    }

    project->validateAll();
    conn->finish();
}

void Server::handleVisitFileMessage(const std::shared_ptr<VisitFileMessage> &message, const std::shared_ptr<Connection> &conn)
{
    uint32_t fileId = 0;
    bool visit = false;

    std::shared_ptr<Project> project = mProjects.value(message->project());
    const uint32_t id = message->sourceFileId();
    if (project && project->isActiveJob(id)) {
        assert(message->file() == message->file().resolved());
        fileId = Location::insertFile(message->file());
        visit = project->visitFile(fileId, id);
    }
    VisitFileResponseMessage msg(fileId, visit);
    conn->send(msg);
}

bool Server::load()
{
    DataFile fileIdsFile(mOptions.dataDir + "fileids", RTags::DatabaseVersion);
    if (fileIdsFile.open(DataFile::Read)) {
        Flags<FileIdsFileFlag> flags;
        fileIdsFile >> flags;
        if (flags & HasSandboxRoot && !Sandbox::hasRoot()) {
            error() << ("This database was produced with --sandbox-root option using relative path. "
                        "You have to specify a sandbox-root argument or wipe the db by running with -C");
            return false;
        } else if (Sandbox::hasRoot() && !(flags & HasSandboxRoot)) {
            error() << ("This database was produced without --sandbox-root option using relative path. "
                        "You can't specify a sandbox-root argument for this db unless you start the db over by passing -C");
            return false;
        }

        if (flags & HasNoRealPath && !(mOptions.options & NoRealPath)) {
            error() << ("This database was produced with --no-realpath and you're running rdm without --no-realpath. "
                        "You must specify --no-realpath argument to use this db or start over by passing -C");
            return false;

        } else if (flags & HasRealPath && mOptions.options & NoRealPath) {
            error() << ("This database was produced without --no-realpath and you're running rdm with --no-realpath. "
                        "You must not specify --no-realpath argument to use this db or start over by passing -C");
            return false;
        }

        // SBROOT
        Hash<Path, uint32_t> pathsToIds;
        fileIdsFile >> pathsToIds;

        Sandbox::decode(pathsToIds);

        if (!Location::init(pathsToIds)) {
            error() << "Corrupted file ids. You have to start over";
            clearProjects(Clear_All);
            return true;
        }
        List<Path> projects = mOptions.dataDir.files(Path::Directory);
        for (size_t i=0; i<projects.size(); ++i) {
            const Path &file = projects.at(i);
            Path filePath = file.mid(mOptions.dataDir.size());
            Path old = filePath;
            if (filePath.endsWith('/'))
                filePath.chop(1);
            RTags::decodePath(filePath);
            if (filePath.isDir()) {
                bool remove = false;
                if (FILE *f = fopen((file + "/project").constData(), "r")) {
                    Deserializer in(f);
                    int version;
                    in >> version;

                    if (version == RTags::DatabaseVersion) {
                        int fs;
                        in >> fs;
                        if (fs != Rct::fileSize(f)) {
                            error("%s seems to be corrupted, refusing to restore. Removing.",
                                  file.constData());
                            remove = true;
                        } else {
                            addProject(filePath.ensureTrailingSlash());
                        }
                    } else {
                        remove = true;
                        error() << file << "has wrong format. Got" << version << "expected" << RTags::DatabaseVersion << "Removing";
                    }
                    fclose(f);
                }
                if (remove) {
                    Path::rmdir(file);
                }
            }
        }
    } else {
        if (!fileIdsFile.error().isEmpty()) {
            error("Can't restore file ids: %s", fileIdsFile.error().constData());
        }
        Hash<Path, IndexParseData> projects;
        mOptions.dataDir.visit([&projects](const Path &path) {
                if (path.isDir()) {
                    Path sources = path + "sources";
                    if (sources.exists()) {
                        Path filePath = path.fileName();
                        if (filePath.endsWith("/"))
                            filePath.chop(1);
                        RTags::decodePath(filePath);
                        if (!filePath.isEmpty()) {
                            String err;
                            IndexParseData data;
                            if (!Project::readSources(sources, data, &err)) {
                                error("Sources restore error %s: %s", path.constData(), err.constData());
                            } else {
                                data.project = filePath;
                                projects[filePath] = std::move(data);
                            }
                        }
                    }
                }
                return Path::Continue;
            });

        clearProjects(Clear_KeepFileIds);
        if (!projects.isEmpty()) {
            error() << "Recovering sources" << projects.size();
        }
        for (auto &s : projects) {
            auto p = addProject(s.first);
            if (p) {
                p->processParseData(std::move(s.second));
                p->save();
            }
        }
        saveFileIds();
    }
    return true;
}

bool Server::saveFileIds()
{
    const uint32_t lastId = Location::lastId();
    if (mLastFileId == lastId)
        return true;
    DataFile fileIdsFile(mOptions.dataDir + "fileids", RTags::DatabaseVersion);
    if (!fileIdsFile.open(DataFile::Write)) {
        error("Can't save file ids: %s", fileIdsFile.error().constData());
        return false;
    }
    Flags<FileIdsFileFlag> flags;
    if (Sandbox::hasRoot())
        flags |= HasSandboxRoot;
    if (mOptions.options & NoRealPath) {
        flags |= HasNoRealPath;
    } else {
        flags |= HasRealPath;
    }

    fileIdsFile << flags << Sandbox::encoded(Location::pathsToIds());

    if (!fileIdsFile.flush()) {
        error("Can't save file ids: %s", fileIdsFile.error().constData());
        return false;
    }

    mLastFileId = lastId;
    return true;
}

void Server::removeSocketFile()
{
#ifdef RTAGS_HAS_LAUNCHD
    if (mOptions.options & Launchd) {
        return;
    }
#endif

    if (getenv("LISTEN_FDS")) {
        return;
    }

    Path::rm(mOptions.socketFile);
}

void Server::stopServers()
{
    removeSocketFile();

    mUnixServer.reset();
}

void Server::codeCompleteAt(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    const Location loc = query->location(Location::CreateLocation);
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project) {
        error("No project found for %s", loc.path().constData());
        conn->finish();
        return;
    }
    const uint32_t fileId = loc.fileId();
    Source source = project->source(fileId, query->buildIndex());
    if (source.isNull()) {
        const Set<uint32_t> deps = project->dependencies(fileId, Project::DependsOnArg);
        if (mCompletionThread) {
            source = mCompletionThread->findSource(deps);
        }

        if (source.isNull()) {
            for (uint32_t dep : deps) {
                source = project->source(dep, query->buildIndex());
                if (!source.isNull())
                    break;
            }
        }

        if (source.isNull()) {
            error("No source found for %s", loc.path().constData());
            conn->finish();
            return;
        }
    }

    if (query->flags() & QueryMessage::CodeCompleteIncludes) {
        project->includeCompletions(query->flags(), conn, std::move(source));
        conn->finish();
        return;
    }

    if (!mCompletionThread) {
        mCompletionThread = new CompletionThread(mOptions.completionCacheSize);
        mCompletionThread->start();
    }

    std::shared_ptr<Connection> c = conn;
    if (!(query->flags() & QueryMessage::SynchronousCompletions)) {
        c->finish();
        c.reset();
    }
    Flags<CompletionThread::Flag> flags;
    if (query->flags() & QueryMessage::Elisp)
        flags |= CompletionThread::Elisp;
    if (query->flags() & QueryMessage::JSON)
        flags |= CompletionThread::JSON;
    if (query->flags() & QueryMessage::XML)
        flags |= CompletionThread::XML;
    if (query->flags() & QueryMessage::CodeCompleteIncludeMacros)
        flags |= CompletionThread::IncludeMacros;
    if (query->flags() & QueryMessage::CodeCompleteNoWait)
        flags |= CompletionThread::NoWait;
    mCompletionThread->completeAt(std::move(source), loc, flags, query->max(), query->unsavedFiles(), query->codeCompletePrefix(), c);
}

void Server::dumpJobs(const std::shared_ptr<Connection> &conn)
{
    mJobScheduler->dumpJobs(conn);
}

void Server::dumpDaemons(const std::shared_ptr<Connection> &conn)
{
    mJobScheduler->dumpDaemons(conn);
}

class TestConnection
{
public:
    TestConnection(const Path &workingDirectory)
        : mConnection(Connection::create(RClient::NumOptions)),
          mIsFinished(false), mWorkingDirectory(workingDirectory)
    {
        mConnection->aboutToSend().connect([this](const std::shared_ptr<Connection> &, const Message *message) {
                if (message->messageId() == Message::FinishMessageId) {
                    mIsFinished = true;
                } else if (message->messageId() == Message::ResponseId) {
                    String response = reinterpret_cast<const ResponseMessage *>(message)->data();
                    if (response.startsWith(mWorkingDirectory)) {
                        response.remove(0, mWorkingDirectory.size());
                    }
                    mOutput.append(response);
                }
            });
    }
    List<String> output() const { return mOutput; }
    bool isFinished() const { return mIsFinished; }
    std::shared_ptr<Connection> connection() const { return mConnection; }
private:
    std::shared_ptr<Connection> mConnection;
    bool mIsFinished;
    List<String> mOutput;
    const Path mWorkingDirectory;
};

bool Server::runTests()
{
    assert(!mOptions.tests.isEmpty());
    bool ret = true;
    int sourceCount = 0;
    mIndexDataMessageReceived.connect([&sourceCount]() {
            // error() << "Got a finish" << sourceCount;
            assert(sourceCount > 0);
            if (!--sourceCount) {
                EventLoop::eventLoop()->quit();
            }
        });
    for (const auto &file : mOptions.tests) {
        const String fileContents = file.readAll();
        if (fileContents.isEmpty()) {
            error() << "Failed to open file" << file;
            ret = false;
            continue;
        }
        bool ok = true;
        const Value value = Value::fromJSON(fileContents, &ok);
        warning() << "parsed json" << value.type() << fileContents.size();
        if (!ok || !value.isMap()) {
            error() << "Failed to parse json" << file << ok << value.type() << value;
            ret = false;
            continue;
        }
        const List<Value> tests = value.operator[]<List<Value> >("tests");
        if (tests.isEmpty()) {
            error() << "Invalid test" << file;
            ret = false;
            continue;
        }
        const List<Value> sources = value.operator[]<List<Value> >("sources");
        if (sources.isEmpty()) {
            error() << "Invalid test" << file;
            ret = false;
            continue;
        }
        warning() << sources.size() << "sources and" << tests.size() << "tests";
        const Path workingDirectory = file.parentDir();
        const Path projectRoot = RTags::findProjectRoot(workingDirectory, RTags::SourceRoot);
        if (projectRoot.isEmpty()) {
            error() << "Can't find project root" << workingDirectory;
            ret = false;
            continue;
        }
        IndexParseData data;
        data.environment = mEnvironment;
        for (const auto &source : sources) {
            if (!source.isString()) {
                error() << "Invalid source" << source;
                ret = false;
                continue;
            }
            data.project = workingDirectory;
            String commands = "clang " + source.convert<String>();
            if (!parse(data, std::move(commands), workingDirectory)) {
                error() << "Failed to index" << ("clang " + source.convert<String>()) << workingDirectory;
                ret = false;
                continue;
            }
            ++sourceCount;
        }
        EventLoop::eventLoop()->exec(mOptions.testTimeout);
        if (sourceCount) {
            error() << "Timed out waiting for sources to compile";
            sourceCount = 0;
            ret = false;
            continue;
        }

        int passes = 0;
        int failures = 0;
        int idx = -1;
        for (const auto &test : tests) {
            ++idx;
            if (!test.isMap()) {
                error() << "Invalid test" << test.type();
                ret = false;
                continue;
            }
            const String type = test.operator[]<String>("type");
            if (type.isEmpty()) {
                error() << "Invalid test. No type";
                ret = false;
                continue;
            }
            std::shared_ptr<QueryMessage> query;
            if (type == "follow-location") {
                String location = Location::encode(test.operator[]<String>("location"), workingDirectory);
                if (location.isEmpty()) {
                    error() << "Invalid test. Invalid location";
                    ret = false;
                    continue;
                }
                query.reset(new QueryMessage(QueryMessage::FollowLocation));
                query->setQuery(std::move(location));
            } else if (type == "references") {
                String location = Location::encode(test.operator[]<String>("location"), workingDirectory);
                if (location.isEmpty()) {
                    error() << "Invalid test. Invalid location";
                    ret = false;
                    continue;
                }
                query.reset(new QueryMessage(QueryMessage::ReferencesLocation));
                query->setQuery(std::move(location));
            } else if (type == "references-name") {
                String name = test.operator[]<String>("name");
                if (name.isEmpty()) {
                    error() << "Invalid test. Invalid name";
                    ret = false;
                    continue;
                }
                query.reset(new QueryMessage(QueryMessage::ReferencesLocation));
                query->setQuery(std::move(name));
            } else {
                error() << "Unknown test" << type;
                ret = false;
                continue;
            }
            const List<Value> flags = test.operator[]<List<Value> >("flags");
            for (const auto &flag : flags) {
                if (!flag.isString()) {
                    error() << "Invalid flag";
                    ret = false;
                } else {
                    const QueryMessage::Flag f = QueryMessage::flagFromString(flag.convert<String>());
                    if (f == QueryMessage::NoFlag) {
                        error() << "Invalid flag";
                        ret = false;
                        continue;
                    }
                    query->setFlag(f);
                }
            }

            TestConnection conn(workingDirectory);
            query->setFlag(QueryMessage::SilentQuery);
            handleQueryMessage(query, conn.connection());
            if (!conn.isFinished()) {
                error() << "Query failed";
                ret = false;
                continue;
            }

            const Value out = test["output"];
            if (!out.isList()) {
                error() << "Invalid output";
                ret = false;
                continue;
            }
            List<String> output;
            for (auto it=out.listBegin(); it != out.listEnd(); ++it) {
                if (!it->isString()) {
                    error() << "Invalid output";
                    ret = false;
                    continue;
                }
                output.append(it->convert<String>());
            }
            if (output != conn.output()) {
                error() << "Test" << idx << "failed. Expected:";
                error() << output;
                error() << "Got:";
                error() << conn.output();
                ret = false;
                ++failures;
            } else {
                warning() << "Test passed";
                ++passes;
            }
        }
        error() << passes << "passes" << failures << "failures" << (tests.size() - failures - passes) << "invalid";
    }

    return ret;
}

void Server::sourceFileModified(const std::shared_ptr<Project> &project, uint32_t fileId)
{
    // error() << Location::path(fileId) << "modified" << (mCompletionThread ? (mCompletionThread->isCached(project, fileId) ? 1 : 0) : -1);
    if (mCompletionThread && mCompletionThread->isCached(project, fileId)) {
        mCompletionThread->reparse(project, fileId);
    }
}

void Server::prepareCompletion(const std::shared_ptr<QueryMessage> &query, uint32_t fileId, const std::shared_ptr<Project> &project)
{
    if (query->flags() & QueryMessage::CodeCompletionEnabled && !mCompletionThread) {
        mCompletionThread = new CompletionThread(mOptions.completionCacheSize);
        mCompletionThread->start();
    }

    if (mCompletionThread && fileId) {
        if (!mCompletionThread->isCached(project, fileId)) {
            Source source = project->source(fileId, query->buildIndex());
            if (source.isNull()) {
                for (const uint32_t dep : project->dependencies(fileId, Project::DependsOnArg)) {
                    source = project->source(dep, query->buildIndex());
                    if (!source.isNull())
                        break;
                }
            }

            if (!source.isNull())
                mCompletionThread->prepare(std::move(source), query->unsavedFiles());
        }
    }
}

void Server::filterBlockedArguments(Source &source)
{
    for (const String &blocked : mOptions.blockedArguments) {
        if (blocked.endsWith("=")) {
            size_t i = 0;
            while (i<source.arguments.size()) {
                if (source.arguments.at(i).startsWith(blocked)) {
                    // error() << "Removing" << source.arguments.at(i);
                    source.arguments.remove(i, 1);
                } else if (!strncmp(blocked.constData(), source.arguments.at(i).constData(), blocked.size() - 1)) {
                    const size_t count = (i + 1 < source.arguments.size()) ? 2 : 1;
                    // error() << "Removing" << source.arguments.mid(i, count);
                    source.arguments.remove(i, count);
                } else {
                    ++i;
                }
            }
        } else {
            source.arguments.remove(blocked);
        }
    }
}
