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

#include "rct/QuitMessage.h"
#include "rct/Message.h"
#include "CompletionThread.h"
#include "Match.h"
#include "Preprocessor.h"
#include "Project.h"
#include "RClient.h"
#include "IndexParseData.h"
#include "rct/Connection.h"
#include "rct/DataFile.h"
#include "rct/EventLoop.h"
#include "rct/Log.h"
#include "rct/Path.h"
#include "rct/Process.h"
#include "rct/Rct.h"
#include "rct/SocketClient.h"
#include "rct/Value.h"
#include "ClangThread.h"
#include "FileManager.h"
#include "Filter.h"
#include "RTags.h"
#include "RTagsLogOutput.h"
#include "Source.h"
#include "RTagsVersion.h"
#include "FileMap.h"
#include "Location.h"
#include "Sandbox.h"
#include "Symbol.h"
#include "clang-c/CXString.h"
#include "rct/Map.h"
#include "rct/Serializer.h"
#include "rct/SocketServer.h"
#include "rct/Thread.h"
#include "JobScheduler.h"

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
        mOptions.defaultArguments.push_back("-Wno-unknown-warning-option");

    mOptions.defines << Source::Define("RTAGS", String(), Source::Define::NoValue);

    if (mOptions.options & EnableCompilerManager) {
#ifndef OS_Darwin   // this causes problems on MacOS+clang
        // http://clang.llvm.org/compatibility.html#vector_builtins
        const char *gccBuiltIntVectorFunctionDefines[] = {
            "__builtin_ia32_rolhi(...)",
            "__builtin_ia32_pause(...)",
            "__builtin_ia32_addcarryx_u32(...)",
            "__builtin_ia32_bsrsi(...)",
            "__builtin_ia32_rdpmc(...)",
            "__builtin_ia32_rdtsc(...)",
            "__builtin_ia32_rdtscp(...)",
            "__builtin_ia32_rolqi(...)",
            "__builtin_ia32_rorqi(...)",
            "__builtin_ia32_rorhi(...)",
            "__builtin_ia32_rolhi(...)",
            "__builtin_ia32_rdseed_di_step(...)",
            "__builtin_ia32_xsaveopt(...)",
            "__builtin_ia32_xsaveopt64(...)",
            "__builtin_ia32_sbb_u32(...)"
        };
        for (const char *def : gccBuiltIntVectorFunctionDefines) {
            mOptions.defines << Source::Define(def);
        }
#endif
    }
#ifdef CLANG_INCLUDE
    mOptions.includePaths.push_back(Source::Include(Source::Include::Type_System, CLANG_INCLUDE_STR));
#endif

    if (!(mOptions.options & NoLibClangIncludePath)) {
        // Iterate until we find an existing directory
        for (Path systemInclude : sSystemIncludePaths) {
            systemInclude = systemInclude.ensureTrailingSlash();
            systemInclude << "clang/" << CLANG_VERSION_STRING << "/include/";
            if (systemInclude.isDir()) {
                mOptions.includePaths.push_back(Source::Include(Source::Include::Type_System, systemInclude));
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
        if (!mOptions.includePaths.empty()) {
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
        List<String> current = Path(mOptions.dataDir + ".currentProject").readAll(1024).split('\n');
        bool found = false;
        if (current.size() == 3) {
            Path path = current[0];
            RTags::decodePath(path);
            const uint32_t compileCommandsFileId = atoi(current[1].constData());
            for (const auto &proj : mProjects.value(path)) {
                if (compileCommandsFileId == proj->compileCommandsFileId()) {
                    found = true;
                    setCurrentProject(proj);
                    break;
                }
            }
        }
        if (!found && !current.empty()) {
            error() << "Can't restore current project" << current;
            unlink((mOptions.dataDir + ".currentProject").constData());
        }
    }

    assert(mOptions.pollTimer >= 0);
    if (mOptions.pollTimer) {
        mPollTimer = EventLoop::eventLoop()->registerTimer([this](int) {
            for (const auto &projects : mProjects) {
                for (const auto &proj : projects.second) {
                    proj->validateAll();
                }
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

std::shared_ptr<Project> Server::addProject(const Path &path, uint32_t compileCommandsFileId)
{
    List<std::shared_ptr<Project>> &projects = mProjects[path];
    for (const auto &project : projects) {
        if (project->compileCommandsFileId() == compileCommandsFileId) {
            return project;
        }
    }
    auto proj = std::make_shared<Project>();
    if (!proj->init(path, compileCommandsFileId)) {
        Path::rmdir(proj->projectDataDir());
        if (projects.empty()) {
            mProjects.erase(path);
            return nullptr;
        }
    }
    projects.push_back(proj);
    return proj;
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

String Server::guessArguments(const String &args, const Path &pwd, const Path &projectRootOverride) const
{
    Set<Path> includePaths;
    List<String> ret;
    bool hasInput = false;
    Set<Path> roots;
    if (!projectRootOverride.empty())
        roots.insert(projectRootOverride.ensureTrailingSlash());
    ret << "/usr/bin/g++"; // this should be clang on mac
    const List<String> split = args.split(" ");
    for (size_t i=0; i<split.size(); ++i) {
        const String &s = split.at(i);
        if (s == "--build-root") {
            const Path root = split.value(++i);
            if (!root.empty())
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
                if (!projectRoot.empty())
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
                    assert(!p.empty());
                    if (!includePaths.insert(p) || p == root)
                        break;
                    p = p.parentDir();
                } while (!p.empty());
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
        assert(!p.empty());
        ret << ("-I" + p);
    }

    return String::join(ret, ' ');
}

bool Server::loadCompileCommands(IndexParseData &data, const Path &compileCommands, const List<String> &environment, SourceCache *cache) const
{
    if (Sandbox::hasRoot() && !data.project.empty() && !data.project.startsWith(Sandbox::root())) {
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
    assert(!data.compileCommandsFileId);
    data.compileCommandsFileId = fileId;
    data.environment = environment;
    data.lastModifiedMs = compileCommands.lastModifiedMs();
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
        ret = parse(data, args, compileDir.ensureTrailingSlash(), fileId, cache) || ret;
    }
    clang_CompileCommands_dispose(cmds);
    clang_CompilationDatabase_dispose(db);
    if (!ret) {
        data.clear();
    }
    return ret;
}

bool Server::parse(IndexParseData &data,
                   String arguments,
                   const Path &pwd,
                   uint32_t compileCommandsFileId,
                   SourceCache *cache) const
{
    if (Sandbox::hasRoot() && !data.project.empty() && !data.project.startsWith(Sandbox::root())) {
        error("Invalid --project-root '%s', must be inside --sandbox-root '%s'",
              data.project.constData(), Sandbox::root().constData());
        return false;
    }

    assert(pwd.endsWith('/'));
    List<Path> unresolvedPaths;
    if (!mOptions.argTransform.empty()) {
        Process process;
        if (process.exec(mOptions.argTransform, List<String>() << arguments) == Process::Done) {
            if (process.returnCode() != 0) {
                warning() << "--arg-transform returned" << process.returnCode() << "for" << arguments;
                return false;
            }
            String stdOut = process.readAllStdOut();
            if (!stdOut.empty() && stdOut != arguments) {
                warning() << "Changed\n" << arguments << "\nto\n" << stdOut;
                arguments = std::move(stdOut);
            }
        }
    }

    assert(data.compileCommandsFileId == compileCommandsFileId);
    SourceList sources = Source::parse(arguments, pwd, data.environment, &unresolvedPaths, cache);
    bool ret = (sources.empty() && unresolvedPaths.size() == 1 && unresolvedPaths.front() == "-");
    debug() << "Got" << sources.size() << "sources, and" << unresolvedPaths << "from" << arguments;
    size_t idx = 0;
    for (Source &source : sources) {
        const Path path = source.sourceFile();

        if (data.project.empty()) {
            if (compileCommandsFileId) {
                for (const auto &projects : mProjects) {
                    for (const auto &p : projects.second) {
                        if (p->compileCommandsFileId() == compileCommandsFileId) {
                            data.project = p->path();
                            break;
                        }
                    }
                }
            }
            if (data.project.empty()) {
                std::shared_ptr<Project> current = currentProject();
                const Path unresolvedPath = unresolvedPaths.at(idx++);
                if (current && (current->match(unresolvedPath) || (path != unresolvedPath && current->match(path)))) {
                    data.project = current->path();
                } else {
                    for (const auto &projs : mProjects) {
                        for (const auto &proj : projs.second) {
                            if (proj->match(unresolvedPath) || (path != unresolvedPath && proj->match(path))) {
                                data.project = proj->path();
                                break;
                            }
                        }
                    }
                }

                if (data.project.empty()) {
                    data.project = RTags::findProjectRoot(unresolvedPath, RTags::SourceRoot, cache);
                    if (data.project.empty() && path != unresolvedPath) {
                        data.project = RTags::findProjectRoot(path, RTags::SourceRoot, cache);
                    }
                }
                data.project.resolve(Path::RealPath, pwd);
            }
        }

        if (shouldIndex(source, data.project)) {
            source.compileCommandsFileId = compileCommandsFileId;
            auto &list = data.sources[source.fileId];
            if (!list.contains(source))
                list.push_back(source);
            ret = true;
        } else {
            debug() << "Shouldn't index" << source;
        }
    }
    return ret;
}

void Server::clearProjects(ClearMode mode)
{
    Path::rmdir(mOptions.dataDir);
    setCurrentProject(std::shared_ptr<Project>());
    for (const auto &p : mProjects) {
        for (const auto &project : p.second) {
            project->destroy();
        }
    }
    mProjects.clear();
    if (mode == Clear_All)
        Location::init(Hash<Path, uint32_t>());
}

std::shared_ptr<Project> Server::findProject(uint32_t fileId)
{
    if (currentProject() && currentProject()->isIndexed(fileId)) {
        return currentProject();
    }

    for (const auto &p : mProjects) {
        for (const auto &proj : p.second) {
            if (proj->isIndexed(fileId)) {
                setCurrentProject(proj);
                return proj;
            }
        }
    }
    return nullptr;
}

bool Server::shouldIndex(const Source &source, const Path &srcRoot) const
{
    if (srcRoot.empty()) {
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

    if (Sandbox::hasRoot() && !srcRoot.empty() && !srcRoot.startsWith(Sandbox::root())) {
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
                fprintf(f, "%s\n%u\n", p.constData(), project->compileCommandsFileId());
                fclose(f);
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
    if (!query->currentFile().empty())
        matches << query->currentFile();

    return projectForMatches(matches);
}

std::shared_ptr<Project> Server::projectForMatches(const List<Match> &matches)
{
    std::shared_ptr<Project> best;
    std::shared_ptr<Project> cur = currentProject();
    // give current a chance first to avoid switching project when using system headers etc
    for (const Match &match : matches) {
        bool isIndexed = false;
        if (cur && cur->match(match, &isIndexed)) {
            if (isIndexed)
                return cur;
            best = cur;
        }

        for (const auto &p : mProjects) {
            for (const auto &project : p.second) {
                if (project != cur && project->match(match, &isIndexed)) {
                    if (isIndexed) {
                        setCurrentProject(project);
                        error() << "Found project" << Location::path(project->compileCommandsFileId());
                        return project;
                    } else if (!best) {
                        best = project;
                    }
                }
            }
        }
    }
    if (best && best != cur)
        setCurrentProject(best);
    return best;
}

void Server::removeProject(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn)
{
    const Match match = query->match();
    auto it = mProjects.begin();
    bool found = false;
    while (it != mProjects.end()) {
        auto pit = it->second.begin();
        while (pit != it->second.end()) {
            const auto &project = *pit;
            if (project->match(match)) {
                found = true;
                if (currentProject() == project) {
                    setCurrentProject(nullptr);
                }
                Path path = it->first;
                conn->write<128>("Deleted project: %s", path.constData());
                RTags::encodePath(path);
                Path::rmdir(mOptions.dataDir + path);
                warning() << "Deleted" << (mOptions.dataDir + path);
                project->destroy();
                pit = it->second.erase(pit);
            } else {
                ++pit;
            }
        }
        if (it->second.empty()) {
            it = mProjects.erase(it);
        } else {
            ++pit;
        }
    }
    if (!found) {
        conn->write<128>("No projects matching %s", match.pattern().constData());
    }
    conn->finish();
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
            bool remove = true;
            char *end = nullptr;
            uint32_t fileId = strtoul(filePath.fileName(), &end, 10);
            if (!*end) {
                filePath = filePath.parentDir();
                if (filePath.isDir()) {
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
                            } else {
                                remove = false;
                                addProject(filePath.ensureTrailingSlash(), fileId);
                            }
                        } else {
                            error() << file << "has wrong format. Got" << version << "expected" << RTags::DatabaseVersion << "Removing";
                        }
                        fclose(f);
                    }
                }
            }
            if (remove) {
                Path::rmdir(file);
            }
        }
        return true;
    }

    if (!fileIdsFile.error().empty()) {
        error("Can't restore file ids: %s", fileIdsFile.error().constData());
    }
    Hash<Path, List<IndexParseData>> projects;
    mOptions.dataDir.visit([&projects](const Path &path) {
        if (path.isDir()) {
            Path sources = path + "sources";
            if (sources.exists()) {
                Path parentDir = path.parentDir();
                Path filePath = parentDir.fileName();
                if (filePath.endsWith("/"))
                    filePath.chop(1);
                RTags::decodePath(filePath);
                if (!filePath.empty()) {
                    String err;
                    IndexParseData data;
                    if (!Project::readSources(sources, data, &err)) {
                        error("Sources restore error %s: %s", path.constData(), err.constData());
                    } else {
                        char *end = nullptr;
                        data.compileCommandsFileId = strtoul(parentDir.fileName(), &end, 10);
                        if (!*end) {
                            projects[filePath].push_back(std::move(data));
                        }
                    }
                }
            }
        }
        return Path::Continue;
    });

    clearProjects(Clear_KeepFileIds);
    if (!projects.empty()) {
        error() << "Recovering sources" << projects.size();
    }
    for (auto &s : projects) {
        for (auto &pp : s.second) {
            auto p = addProject(s.first, pp.compileCommandsFileId);
            if (p) {
                p->processParseData(std::move(pp));
                p->save();
            }
        }
    }
    saveFileIds();
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
                mOutput.push_back(response);
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
    assert(!mOptions.tests.empty());
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
        if (fileContents.empty()) {
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
        if (tests.empty()) {
            error() << "Invalid test" << file;
            ret = false;
            continue;
        }
        const List<Value> sources = value.operator[]<List<Value> >("sources");
        if (sources.empty()) {
            error() << "Invalid test" << file;
            ret = false;
            continue;
        }
        warning() << sources.size() << "sources and" << tests.size() << "tests";
        const Path workingDirectory = file.parentDir();
        const Path projectRoot = RTags::findProjectRoot(workingDirectory, RTags::SourceRoot);
        if (projectRoot.empty()) {
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
            if (!parse(data, std::move(commands), workingDirectory, 0)) {
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
            if (type.empty()) {
                error() << "Invalid test. No type";
                ret = false;
                continue;
            }
            std::shared_ptr<QueryMessage> query;
            if (type == "follow-location") {
                String location = Location::encode(test.operator[]<String>("location"), workingDirectory);
                if (location.empty()) {
                    error() << "Invalid test. Invalid location";
                    ret = false;
                    continue;
                }
                query.reset(new QueryMessage(QueryMessage::FollowLocation));
                query->setQuery(std::move(location));
            } else if (type == "references") {
                String location = Location::encode(test.operator[]<String>("location"), workingDirectory);
                if (location.empty()) {
                    error() << "Invalid test. Invalid location";
                    ret = false;
                    continue;
                }
                query.reset(new QueryMessage(QueryMessage::ReferencesLocation));
                query->setQuery(std::move(location));
            } else if (type == "references-name") {
                String name = test.operator[]<String>("name");
                if (name.empty()) {
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
                output.push_back(it->convert<String>());
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

