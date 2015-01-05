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

#include "Server.h"

#include "CompletionThread.h"
#include "IndexMessage.h"
#include "LogOutputMessage.h"
#include "CursorInfoJob.h"
#include "DependenciesJob.h"
#include "VisitFileResponseMessage.h"
#include "Filter.h"
#include "FindFileJob.h"
#include "FindSymbolsJob.h"
#include "FollowLocationJob.h"
#include "IndexerJob.h"
#include "Source.h"
#include "DumpThread.h"
#include "DataFile.h"
#if CLANG_VERSION_MAJOR > 3 || (CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR > 3)
#include <clang-c/CXCompilationDatabase.h>
#endif
#include "ListSymbolsJob.h"
#include "LogObject.h"
#include "Match.h"
#include "Preprocessor.h"
#include "Project.h"
#include "JobScheduler.h"
#include "QueryMessage.h"
#include "VisitFileMessage.h"
#include "IndexerMessage.h"
#include "RTags.h"
#include "ReferencesJob.h"
#include "StatusJob.h"
#include <clang-c/Index.h>
#include <rct/Connection.h>
#include <rct/Value.h>
#include <rct/EventLoop.h>
#include <rct/SocketClient.h>
#include <rct/Log.h>
#include <rct/Message.h>
#include <rct/Path.h>
#include <rct/Process.h>
#include <rct/Rct.h>
#include <rct/RegExp.h>
#include <stdio.h>
#include <arpa/inet.h>
#include <limits>

#if not defined CLANG_INCLUDEPATH
#error CLANG_INCLUDEPATH not defined during CMake generation
#else
#define TO_STR1(x) #x
#define TO_STR(x)  TO_STR1(x)
#define CLANG_INCLUDEPATH_STR TO_STR(CLANG_INCLUDEPATH)
#endif

class HttpLogObject : public LogOutput
{
public:
    HttpLogObject(int logLevel, const SocketClient::SharedPtr &socket)
        : LogOutput(logLevel), mSocket(socket)
    {}

    virtual bool testLog(int level) const
    {
        return level == logLevel();
    }
    virtual void log(const char *msg, int len)
    {
        if (!EventLoop::isMainThread()) {
            String message(msg, len);
            SocketClient::WeakPtr weak = mSocket;

            EventLoop::eventLoop()->callLater(std::bind([message, weak, this] {
                        // ### At some point (with some version of GCC) this
                        // ### lambda wouldn't compile unless "this" was
                        // ### captured.
                        if (SocketClient::SharedPtr socket = weak.lock()) {
                            HttpLogObject::send(message.constData(), message.size(), socket);
                        }
                    }));
        } else {
            send(msg, len, mSocket);
        }
    }
    static void send(const char *msg, int len, const SocketClient::SharedPtr &socket)
    {
        static const unsigned char *header = reinterpret_cast<const unsigned char*>("data:");
        static const unsigned char *crlf = reinterpret_cast<const unsigned char*>("\r\n");
        socket->write(header, 5);
        socket->write(reinterpret_cast<const unsigned char *>(msg), len);
        socket->write(crlf, 2);
    }
private:
    SocketClient::SharedPtr mSocket;
};

Server *Server::sInstance = 0;
Server::Server()
    : mSuspended(false), mVerbose(false), mLastFileId(0), mCompletionThread(0)
{
    assert(!sInstance);
    sInstance = this;

    mUnloadTimer.timeout().connect(std::bind(&Server::onUnload, this));
}

Server::~Server()
{
    if (mCompletionThread) {
        mCompletionThread->stop();
        mCompletionThread->join();
        delete mCompletionThread;
        mCompletionThread = 0;
    }

    stopServers();
    for (const auto project : mProjects) {
        project.second->unload();
        // need to save updated Source::Enabled
    }
    mProjects.clear(); // need to be destroyed before sInstance is set to 0
    assert(sInstance == this);
    sInstance = 0;
    Message::cleanup();
}

bool Server::init(const Options &options)
{
    RTags::initMessages();

    mOptions = options;
    mSuspended = (options.flag(StartSuspended));
    if (!(options.flag(NoUnlimitedErrors)))
        mOptions.defaultArguments << "-ferror-limit=0";
    if (options.flag(Wall))
        mOptions.defaultArguments << "-Wall";
    if (options.flag(SpellChecking))
        mOptions.defaultArguments << "-fspell-checking";
    if (!(options.flag(NoNoUnknownWarningsOption)))
        mOptions.defaultArguments.append("-Wno-unknown-warning-option");

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
            0
        };
        for (int i=0; gccBuiltIntVectorFunctionDefines[i]; ++i) {
            mOptions.defines << Source::Define(String::format<128>("%s(...)", gccBuiltIntVectorFunctionDefines[i]));
        }
#endif
    } else {
        const Path clangPath = Path::resolved(CLANG_INCLUDEPATH_STR);
        mOptions.includePaths.append(Source::Include(Source::Include::Type_System, clangPath));
    }

    Log l(Error);
    l << "Running with" << mOptions.jobCount << "jobs, using args:"
      << String::join(mOptions.defaultArguments, ' ') << '\n';
    l << "Includepaths:";
    for (const auto &inc : mOptions.includePaths)
        l << inc.toString();

    if (mOptions.options & ClearProjects) {
        clearProjects();
    }

    for (int i=0; i<10; ++i) {
        mUnixServer.reset(new SocketServer);
        warning() << "listening" << mOptions.socketFile;
        if (mUnixServer->listen(mOptions.socketFile)) {
            break;
        }
        mUnixServer.reset();
        if (!i) {
            enum { Timeout = 1000 };
            Connection connection;
            if (connection.connectUnix(mOptions.socketFile, Timeout)) {
                connection.send(QueryMessage(QueryMessage::Shutdown));
                connection.disconnected().connect(std::bind([](){ EventLoop::eventLoop()->quit(); }));
                connection.finished().connect(std::bind([](){ EventLoop::eventLoop()->quit(); }));
                EventLoop::eventLoop()->exec(Timeout);
            }
        } else {
            sleep(1);
        }
        Path::rm(mOptions.socketFile);
    }
    if (!mUnixServer) {
        error("Unable to listen on %s", mOptions.socketFile.constData());
        return false;
    }

    mJobScheduler.reset(new JobScheduler(mOptions.jobCount));

    restoreFileIds();
    mUnixServer->newConnection().connect(std::bind(&Server::onNewConnection, this, std::placeholders::_1));
    reloadProjects();
    if (!(mOptions.options & NoStartupCurrentProject)) {
        Path current = Path(mOptions.dataDir + ".currentProject").readAll(1024);
        if (current.size() > 1) {
            current.chop(1);
            const auto project = mProjects.value(current);
            if (!project) {
                error() << "Can't restore project" << current;
                unlink((mOptions.dataDir + ".currentProject").constData());
            } else {
                setCurrentProject(project);
            }
        }
    }

    return true;
}

std::shared_ptr<Project> Server::addProject(const Path &path) // lock always held
{
    std::shared_ptr<Project> &project = mProjects[path];
    if (!project) {
        project.reset(new Project(path));
        return project;
    }
    return std::shared_ptr<Project>();
}

int Server::reloadProjects()
{
    mProjects.clear(); // ### could keep the ones that persist somehow
    List<Path> projects = mOptions.dataDir.files(Path::File);
    const Path home = Path::home();
    for (int i=0; i<projects.size(); ++i) {
        Path file = projects.at(i);
        Path p = file.mid(mOptions.dataDir.size());
        RTags::decodePath(p);
        if (p.isDir()) {
            bool remove = false;
            if (FILE *f = fopen(file.constData(), "r")) {
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
                        addProject(p);
                    }
                } else {
                    remove = true;
                    error() << file << "has wrong format. Got" << version << "expected" << RTags::DatabaseVersion << "Removing";
                }
                fclose(f);
            }
            if (remove) {
                Path::rm(file);
            }
        }
    }
    return mProjects.size();
}

void Server::onNewConnection(SocketServer *server)
{
    while (true) {
        SocketClient::SharedPtr client = server->nextConnection();
        if (!client)
            break;
        Connection *conn = new Connection(client);
        conn->newMessage().connect(std::bind(&Server::onNewMessage, this, std::placeholders::_1, std::placeholders::_2));
        conn->disconnected().connect(std::bind([conn]() {
                    conn->disconnected().disconnect();
                    EventLoop::deleteLater(conn);
                }));
    }
}

void Server::onNewMessage(const std::shared_ptr<Message> &message, Connection *connection)
{
    if (mOptions.unloadTimer)
        mUnloadTimer.restart(mOptions.unloadTimer * 1000 * 60, Timer::SingleShot);

    std::shared_ptr<RTagsMessage> m = std::static_pointer_cast<RTagsMessage>(message);

    switch (message->messageId()) {
    case IndexMessage::MessageId:
        handleIndexMessage(std::static_pointer_cast<IndexMessage>(m), connection);
        break;
    case QueryMessage::MessageId:
        handleQueryMessage(std::static_pointer_cast<QueryMessage>(m), connection);
        break;
    case IndexerMessage::MessageId:
        handleIndexerMessage(std::static_pointer_cast<IndexerMessage>(m), connection);
        break;
    case LogOutputMessage::MessageId:
        error() << m->raw();
        handleLogOutputMessage(std::static_pointer_cast<LogOutputMessage>(m), connection);
        break;
    case VisitFileMessage::MessageId:
        handleVisitFileMessage(std::static_pointer_cast<VisitFileMessage>(m), connection);
        break;
    case ResponseMessage::MessageId:
    case FinishMessage::MessageId:
    case VisitFileResponseMessage::MessageId:
        error() << "Unexpected message" << static_cast<int>(message->messageId());
        // assert(0);
        connection->finish(1);
        break;
    default:
        error("Unknown message: %d", message->messageId());
        connection->finish(1);
        break;
    }
    if (mOptions.options & NoFileManagerWatch) {
        std::shared_ptr<Project> project = currentProject();
        if (project && project->fileManager && (Rct::monoMs() - project->fileManager->lastReloadTime()) > 60000)
            project->fileManager->reload(FileManager::Asynchronous);
    }
}

bool Server::index(const String &arguments, const Path &pwd, const Path &projectRootOverride, unsigned int flags)
{
    List<Path> unresolvedPaths;
    List<Source> sources;
    bool parse = true;
    if (!mOptions.argTransform.isEmpty()) {
        Process process;
        if (process.exec(mOptions.argTransform, List<String>() << arguments) == Process::Done) {
            if (process.returnCode() != 0) {
                warning() << "--arg-transform returned" << process.returnCode() << "for" << arguments;
                return false;
            }
            const String stdOut = process.readAllStdOut();
            if (stdOut != arguments) {
                warning() << "Changed\n" << arguments << "\nto\n" << stdOut;
                parse = false;
                sources = Source::parse(stdOut, pwd, flags, &unresolvedPaths);
            }
        }
    }

    if (parse)
        sources = Source::parse(arguments, pwd, flags, &unresolvedPaths);

    bool ret = false;
    int idx = 0;
    for (Source &source : sources) {
        const Path path = source.sourceFile();

        std::shared_ptr<Project> current = currentProject();
        Path root;
        const Path unresolvedPath = unresolvedPaths.at(idx++);
        if (current && (current->match(unresolvedPath) || (path != unresolvedPath && current->match(path)))) {
            root = current->path();
        } else {
            for (const auto &proj : mProjects) {
                if (proj.second->match(unresolvedPath) || (path != unresolvedPath && proj.second->match(path))) {
                    root = proj.first;
                    break;
                }
            }
        }

        if (root.isEmpty()) {
            root = projectRootOverride;
            if (root.isEmpty()) {
                root = RTags::findProjectRoot(unresolvedPath, RTags::SourceRoot);
                if (root.isEmpty() && path != unresolvedPath)
                    root = RTags::findProjectRoot(path, RTags::SourceRoot);
            }
        }

        if (shouldIndex(source, root)) {
            std::shared_ptr<Project> &project = mProjects[root];
            if (!project) {
                addProject(root);
                assert(project);
            }
            project->load();
            if (!mCurrentProject.lock())
                setCurrentProject(project);
            project->index(std::shared_ptr<IndexerJob>(new IndexerJob(source, IndexerJob::Compile, root)));
            ret = true;
        }
    }
    return ret;
}

void Server::handleIndexMessage(const std::shared_ptr<IndexMessage> &message, Connection *conn)
{
#if CLANG_VERSION_MAJOR > 3 || (CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR > 3)
    const Path path = message->compilationDatabaseDir();
    if (!path.isEmpty()) {
        CXCompilationDatabase_Error err;
        CXCompilationDatabase db = clang_CompilationDatabase_fromDirectory(path.constData(), &err);
        if (err != CXCompilationDatabase_NoError) {
            if (conn) {
                conn->write("Can't load compilation database");
                conn->finish();
            }
            return;
        }
        CXCompileCommands cmds = clang_CompilationDatabase_getAllCompileCommands(db);
        const unsigned int sz = clang_CompileCommands_getSize(cmds);
        for (unsigned int i = 0; i < sz; ++i) {
            CXCompileCommand cmd = clang_CompileCommands_getCommand(cmds, i);
            String args;
            CXString str = clang_CompileCommand_getDirectory(cmd);
            Path dir = clang_getCString(str);
            clang_disposeString(str);
            const unsigned int num = clang_CompileCommand_getNumArgs(cmd);
            for (unsigned int j = 0; j < num; ++j) {
                str = clang_CompileCommand_getArg(cmd, j);
                args += clang_getCString(str);
                clang_disposeString(str);
                if (j < num - 1)
                    args += " ";
            }

            index(args, dir, message->projectRoot(), message->escape() ? Source::Escape : Source::None);
        }
        clang_CompileCommands_dispose(cmds);
        clang_CompilationDatabase_dispose(db);
        if (conn) {
            conn->write("[Server] Compilation database loaded");
            conn->finish();
        }
        return;
    }
#endif
    const bool ret = index(message->arguments(), message->workingDirectory(),
                           message->projectRoot(), message->escape() ? Source::Escape : Source::None);
    if (conn)
        conn->finish(ret ? 0 : 1);
}

void Server::handleLogOutputMessage(const std::shared_ptr<LogOutputMessage> &message, Connection *conn)
{
    new LogObject(conn, message->level());
}

void Server::handleIndexerMessage(const std::shared_ptr<IndexerMessage> &message, Connection *conn)
{
    mJobScheduler->handleIndexerMessage(message);
    conn->finish();
    mIndexerMessageReceived();
}

void Server::handleQueryMessage(const std::shared_ptr<QueryMessage> &message, Connection *conn)
{
    if (!(message->flags() & QueryMessage::SilentQuery))
        error() << message->raw();
    conn->setSilent(message->flags() & QueryMessage::Silent);

    switch (message->type()) {
    case QueryMessage::Invalid:
        assert(0);
        break;
    case QueryMessage::SyncProject:
        syncProject(message, conn);
        break;
    case QueryMessage::Sources:
        sources(message, conn);
        break;
    case QueryMessage::GenerateTest:
        generateTest(message, conn);
        break;
    case QueryMessage::DumpCompletions:
        dumpCompletions(message, conn);
        break;
    case QueryMessage::DumpCompilationDatabase:
        dumpCompilationDatabase(message, conn);
        break;
    case QueryMessage::SendDiagnostics:
        sendDiagnostics(message, conn);
        break;
    case QueryMessage::CodeCompleteAt:
    case QueryMessage::PrepareCodeCompleteAt:
        codeCompleteAt(message, conn);
        break;
    case QueryMessage::Suspend:
        suspend(message, conn);
        break;
    case QueryMessage::IsIndexing:
        isIndexing(message, conn);
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
        dumpFile(message, conn);
        break;
    case QueryMessage::Dependencies:
        dependencies(message, conn);
        break;
    case QueryMessage::DeleteProject:
        removeProject(message, conn);
        break;
    case QueryMessage::UnloadProject:
        removeProject(message, conn);
        break;
    case QueryMessage::ReloadProjects:
        reloadProjects(message, conn);
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
    case QueryMessage::CursorInfo:
        cursorInfo(message, conn);
        break;
    case QueryMessage::Shutdown:
        shutdown(message, conn);
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
        preprocessFile(message, conn);
        break;
    case QueryMessage::ReloadFileManager:
        reloadFileManager(message, conn);
        break;
    }
}

void Server::followLocation(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    const Location loc = query->location();
    if (loc.isNull()) {
        conn->write("Not indexed");
        conn->finish(1);
        return;
    }
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project) {
        error("No project");
        conn->finish(1);
        return;
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
        conn->finish(2);
        return;
    }

    int ret;
    {
        FollowLocationJob job(loc, query, project);
        ret = job.run(conn);
        if (!ret) {
            conn->finish(ret);
            return;
        }
    }

    /* We will try with another project under the following circumstances:

       - We didn't find anything with the current project
       - The path in question (likely a header) does not start with the current
       project's path (there's room for mistakes here with symlinks).
       - The file in question does start with another project's path
       - The other project is loaded (we will start loading it if it's not)
    */

    const Path path = loc.path();
    if (!path.startsWith(project->path())) {
        for (const auto &proj : mProjects) {
            if (proj.second != project) {
                Path paths[] = { proj.first, proj.first };
                paths[1].resolve();
                for (const Path &projectPath : paths) {
                    if (path.startsWith(projectPath) && !proj.second->load(Project::FileManager_Asynchronous)) {
                        FollowLocationJob job(loc, query, proj.second);
                        ret = job.run(conn);
                        if (!ret) {
                            conn->finish(ret);
                            return;
                        }
                    }
                }
            }
        }
    }
    conn->finish(ret);
}

void Server::isIndexing(const std::shared_ptr<QueryMessage> &, Connection *conn)
{
    for (const auto &it : mProjects) {
        if (it.second->isIndexing()) {
            conn->write("1");
            conn->finish();
            return;
        }
    }
    conn->write("0");
    conn->finish();
}

void Server::removeFile(const std::shared_ptr<QueryMessage> &query, Connection *conn)
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
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
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

void Server::findFile(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    std::shared_ptr<Project> project = currentProject();
    if (!project || project->state() == Project::Unloaded) {
        error("No project");
        conn->finish();
        return;
    }

    FindFileJob job(query, project);
    const int ret = job.run(conn);
    conn->finish(ret);
}

void Server::dumpFile(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    const uint32_t fileId = Location::fileId(query->query());
    if (!fileId) {
        conn->write<256>("%s is not indexed", query->query().constData());
        conn->finish();
        return;
    }

    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project || project->state() != Project::Loaded) {
        conn->write<256>("%s is not indexed", query->query().constData());
        conn->finish();
        return;
    }

    const Source source = project->sources(fileId).value(query->buildIndex());
    if (!source.isNull()) {
        conn->disconnected().disconnect();
        // ### this is a hack, but if the connection goes away we can't post
        // ### events to it. We could fix this nicer but I didn't

        DumpThread *dumpThread = new DumpThread(query, source, conn);
        dumpThread->start(Thread::Normal, 8 * 1024 * 1024); // 8MiB stack size
    } else {
        conn->write<256>("%s build: %d not found", query->query().constData(), query->buildIndex());
        conn->finish();
    }
}

void Server::generateTest(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    const uint32_t fileId = Location::fileId(query->query());
    if (!fileId) {
        conn->write<256>("%s is not indexed", query->query().constData());
        conn->finish();
        return;
    }

    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project || project->state() != Project::Loaded) {
        conn->write<256>("%s is not indexed", query->query().constData());
        conn->finish();
        return;
    }

    const Source source = project->sources(fileId).value(query->buildIndex());
    if (!source.isNull()) {
        enum CommandLineMode {
            IncludeCompiler = 0x001,
            IncludeSourceFile = 0x002,
            IncludeDefines = 0x004,
            IncludeIncludepaths = 0x008,
            QuoteDefines = 0x010,
            FilterBlacklist = 0x020,
            ExcludeDefaultArguments = 0x040,
            ExcludeDefaultIncludePaths = 0x080,
            ExcludeDefaultDefines = 0x100,
            Default = IncludeDefines|IncludeIncludepaths|FilterBlacklist
        };

        const unsigned int flags = (Source::Default
                                    |Source::ExcludeDefaultDefines
                                    |Source::ExcludeDefaultIncludePaths
                                    |Source::ExcludeDefaultArguments);

        const Path root = source.sourceFile().parentDir().ensureTrailingSlash();
        List<String> compile = source.toCommandLine(flags);
        for (auto &ref : compile) {
            const int idx = ref.indexOf(root);
            if (idx != -1)
                ref.remove(idx, root.size());
        }
        compile.append(source.sourceFile().fileName());

        Value out;
        out["sources"] = (List<Value>() << String::join(compile, ' '));

        List<Value> tests;

        const SymbolMap &map = project->symbols();
        List<Value> noContextFlags;
        noContextFlags.append("no-context");
        auto it = map.lower_bound(Location(source.fileId, 0, 0));
        while (it != map.end() && it->first.fileId() == source.fileId) {
            Location loc;
            if (it->second->bestTarget(map, &loc) && loc.fileId() == source.fileId) {
                Map<String, Value> test;
                test["type"] = "follow-location";
                test["flags"] = noContextFlags;
                test["location"] = String::format<128>("%s:%d:%d:", it->first.path().fileName(), it->first.line(), it->first.column());
                List<Value> output;
                output.append(String::format<128>("%s:%d:%d:", loc.path().fileName(), loc.line(), loc.column()));
                test["output"] = output;
                tests.append(test);
            }
            ++it;
        }
        out["tests"] = tests;
        conn->finish(out.toJSON(true));
    } else {
        conn->write<256>("%s build: %d not found", query->query().constData(), query->buildIndex());
        conn->finish();
    }
}

void Server::cursorInfo(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    const Location loc = query->location();
    if (loc.isNull()) {
        conn->finish();
        return;
    }
    std::shared_ptr<Project> project = projectForQuery(query);

    if (!project) {
        conn->finish();
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
        conn->finish();
    } else {
        CursorInfoJob job(loc, query, project);
        const int ret = job.run(conn);
        conn->finish(ret);
    }
}

void Server::dependencies(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project) {
        conn->finish();
        return;
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
        conn->finish();
        return;
    }

    DependenciesJob job(query, project);
    const int ret = job.run(conn);
    conn->finish(ret);
}

void Server::fixIts(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    std::shared_ptr<Project> project = projectForQuery(query);
    if (project && project->state() == Project::Loaded) {
        String out = project->fixIts(Location::fileId(query->query()));
        if (!out.isEmpty())
            conn->write(out);
    }
    conn->finish();
}

void Server::referencesForLocation(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    const Location loc = query->location();
    if (loc.isNull()) {
        conn->write("Not indexed");
        conn->finish();
        return;
    }
    std::shared_ptr<Project> project = projectForQuery(query);

    if (!project) {
        error("No project");
        conn->finish();
        return;
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
        conn->finish();
        return;
    }

    ReferencesJob job(loc, query, project);
    const int ret = job.run(conn);
    conn->finish(ret);
}

void Server::referencesForName(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    const String name = query->query();

    std::shared_ptr<Project> project = currentProject();

    if (!project) {
        error("No project");
        conn->finish();
        return;
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
        conn->finish();
        return;
    }

    ReferencesJob job(name, query, project);
    const int ret = job.run(conn);
    conn->finish(ret);
}

void Server::findSymbols(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    const String partial = query->query();

    std::shared_ptr<Project> project = currentProject();

    if (!project) {
        error("No project");
        conn->finish();
        return;
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
        conn->finish();
        return;
    }

    FindSymbolsJob job(query, project);
    const int ret = job.run(conn);
    conn->finish(ret);
}

void Server::listSymbols(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    const String partial = query->query();

    std::shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    ListSymbolsJob job(query, project);
    const int ret = job.run(conn);
    conn->finish(ret);
}

void Server::status(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    std::shared_ptr<Project> project = currentProject();

    if (!project) {
        error("No project");
        conn->finish();
        return;
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
        conn->finish();
        return;
    }

    conn->client()->setWriteMode(SocketClient::Synchronous);

    StatusJob job(query, project);
    const int ret = job.run(conn);
    conn->finish(ret);
}

void Server::isIndexed(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    String ret = "unknown";
    const Match match = query->match();
    std::shared_ptr<Project> project = projectForQuery(query);
    if (project) {
        bool indexed = false;
        if (project->match(match, &indexed))
            ret = indexed ? "indexed" : "managed";
    }

    if (!(query->flags() & QueryMessage::SilentQuery))
        error("=> %s", ret.constData());
    conn->write(ret);
    conn->finish();
}

void Server::reloadFileManager(const std::shared_ptr<QueryMessage> &, Connection *conn)
{
    std::shared_ptr<Project> project = currentProject();
    if (project) {
        conn->write<512>("Reloading files for %s", project->path().constData());
        conn->finish();
        project->fileManager->reload(FileManager::Asynchronous);
    } else {
        conn->write("No current project");
        conn->finish();
    }
}

void Server::hasFileManager(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    const Path path = query->query();
    std::shared_ptr<Project> project = projectForQuery(query);
    if (project && project->fileManager && (project->fileManager->contains(path) || project->match(query->match()))) {
        if (!(query->flags() & QueryMessage::SilentQuery))
            error("=> 1");
        conn->write("1");
    } else {
        if (!(query->flags() & QueryMessage::SilentQuery))
            error("=> 0");
        conn->write("0");
    }
    conn->finish();
}

void Server::preprocessFile(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    const Path path = query->query();
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project) {
        conn->write("No project");
        conn->finish();
        return;
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
        conn->finish();
        return;
    }

    const uint32_t fileId = Location::fileId(path);
    const Source source = project->sources(fileId).value(query->buildIndex());
    if (!source.isValid()) {
        conn->write<256>("%s build: %d not found", query->query().constData(), query->buildIndex());
        conn->finish();
    } else {
        Preprocessor *pre = new Preprocessor(source, conn);
        pre->preprocess();
    }
}

void Server::clearProjects()
{
    for (const auto &it : mProjects)
        it.second->unload();
    Rct::removeDirectory(mOptions.dataDir);
    setCurrentProject(std::shared_ptr<Project>());
    mProjects.clear();
}

void Server::reindex(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    Match match = query->match();
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project) {
        project = currentProject();
        if (!project) {
            error("No project");
            conn->finish();
            return;
        } else if (project->state() != Project::Loaded) {
            conn->write("Project loading");
            conn->finish();
            return;
        }
    }

    const int count = project->reindex(match, query);
    // error() << count << query->query();
    if (count) {
        conn->write<128>("Dirtied %d files", count);
    } else {
        conn->write("No matches");
    }
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
        warning() << "Shouldn't index" << source.sourceFile() << "because of exclude filter";
        return false;
    }

    return true;
}

void Server::setCurrentProject(const std::shared_ptr<Project> &project, unsigned int queryFlags)
{
    std::shared_ptr<Project> old = currentProject();
    if (project != old) {
        if (old && old->fileManager)
            old->fileManager->clearFileSystemWatcher();
        mCurrentProject = project;
        if (project) {
            Path::mkdir(mOptions.dataDir);
            FILE *f = fopen((mOptions.dataDir + ".currentProject").constData(), "w");
            if (f) {
                if (!fwrite(project->path().constData(), project->path().size(), 1, f) || !fwrite("\n", 1, 1, f)) {
                    error() << "error writing to" << (mOptions.dataDir + ".currentProject");
                    fclose(f);
                    unlink((mOptions.dataDir + ".currentProject").constData());
                } else {
                    fclose(f);
                }
            } else {
                error() << "error opening" << (mOptions.dataDir + ".currentProject") << "for write";
            }
            Project::FileManagerMode mode = Project::FileManager_Asynchronous;
            if (queryFlags & QueryMessage::WaitForLoadProject)
                mode = Project::FileManager_Synchronous;
            switch (project->state()) {
            case Project::Loaded:
            case Project::Inited:
                project->fileManager->reload(FileManager::Asynchronous);
                break;
            default:
                break;
            }
            project->load(mode);
        } else {
            Path::rm(mOptions.dataDir + ".currentProject");
        }
    }
}

std::shared_ptr<Project> Server::projectForQuery(const std::shared_ptr<QueryMessage> &query)
{
    Match matches[2];
    if (query->flags() & QueryMessage::HasLocation) {
        matches[0] = query->location().path();
    } else {
        matches[0] = query->match();
    }
    matches[1] = query->currentFile();
    std::shared_ptr<Project> cur = currentProject();
    // give current a chance first to avoid switching project when using system headers etc
    for (int i=0; i<2; ++i) {
        const Match &match = matches[i];
        if (cur && cur->match(match))
            return cur;

        for (const auto &it : mProjects) {
            if (it.second != cur && it.second->match(match)) {
                setCurrentProject(it.second, query->flags());
                return it.second;
            }
        }
    }
    return std::shared_ptr<Project>();
}

void Server::removeProject(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    const bool unload = query->type() == QueryMessage::UnloadProject;

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
            cur->second->unload();
            Path path = cur->first;
            conn->write<128>("%s project: %s", unload ? "Unloaded" : "Deleted", path.constData());
            if (!unload) {
                RTags::encodePath(path);
                Path::rm(mOptions.dataDir + path);
                mProjects.erase(cur);
            }
        }
    }
    if (!found) {
        conn->write<128>("No projects matching %s", match.pattern().constData());
    }
    conn->finish();
}

void Server::reloadProjects(const std::shared_ptr<QueryMessage> &/*query*/, Connection *conn)
{
    const int old = mProjects.size();
    const int cur = reloadProjects();
    conn->write<128>("Changed from %d to %d projects", old, cur);
    conn->finish();
}

void Server::project(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    if (query->query().isEmpty()) {
        const std::shared_ptr<Project> current = currentProject();
        const char *states[] = { "(unloaded)", "(inited)", "(loading)", "(loaded)", "(syncing)" };
        for (const auto &it : mProjects) {
            conn->write<128>("%s %s%s", it.first.constData(), states[it.second->state()], it.second == current ? " <=" : "");
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
            for (const auto &pit : mProjects) {
                assert(pit.second);
                if (ok) {
                    if (!index) {
                        selected = pit.second;
                    } else {
                        --index;
                    }
                }
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

void Server::jobCount(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    if (query->query().isEmpty()) {
        conn->write<128>("Running with %d jobs", mOptions.jobCount);
    } else {
        const int jobCount = query->query().toLongLong();
        if (jobCount < 0 || jobCount > 100) {
            conn->write<128>("Invalid job count %s (%d)", query->query().constData(), jobCount);
        } else {
            mOptions.jobCount = jobCount;
            mJobScheduler->setMaxJobs(jobCount);
            conn->write<128>("Changed jobs to %d", jobCount);
        }
    }
    conn->finish();
}

void Server::sendDiagnostics(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    if (testLog(RTags::CompilationErrorXml))
        logDirect(RTags::CompilationErrorXml, query->query());
    conn->finish();
}

void Server::clearProjects(const std::shared_ptr<QueryMessage> &/*query*/, Connection *conn)
{
    clearProjects();
    conn->write("Cleared projects");
    conn->finish();
}

void Server::shutdown(const std::shared_ptr<QueryMessage> &/*query*/, Connection *conn)
{
    for (const auto &it : mProjects) {
        if (it.second)
            it.second->unload();
    }
    EventLoop::eventLoop()->quit();
    conn->write("Shutting down");
    conn->finish();
}

void Server::sources(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    const Path path = query->query();
    const bool flagsOnly = query->flags() & QueryMessage::CompilationFlagsOnly;
    const bool splitLine = query->flags() & QueryMessage::CompilationFlagsSplitLine;
    if (path.isFile()) {
        std::shared_ptr<Project> project = projectForQuery(query);
        if (project) {
            if (project->state() != Project::Loaded) {
                conn->write("Project loading");
            } else {
                const uint32_t fileId = Location::fileId(path);
                if (fileId) {
                    const List<Source> sources = project->sources(fileId);
                    int idx = 0;
                    for (const auto &it : sources) {
                        String out;
                        if (sources.size() > 1)
                            out = String::format<4>("%d: ", idx);
                        if (flagsOnly) {
                            out += String::join(it.toCommandLine(0), splitLine ? '\n' : ' ');
                        } else {
                            out += it.toString();
                        }
                        conn->write(out);
                    }
                }
            }
            conn->finish();
            return;
        }
    }

    if (std::shared_ptr<Project> project = currentProject()) {
        const Match match = query->match();
        if (project->state() != Project::Loaded) {
            conn->write("Project loading");
        } else {
            const SourceMap infos = project->sources();
            for (const auto &it : infos) {
                if (match.isEmpty() || match.match(it.second.sourceFile())) {
                    if (flagsOnly) {
                        conn->write<128>("%s%s%s",
                                         it.second.sourceFile().constData(),
                                         splitLine ? "\n" : ": ",
                                         String::join(it.second.toCommandLine(0), splitLine ? '\n' : ' ').constData());
                    } else {
                        conn->write(it.second.toString());
                    }
                }
            }
        }
    } else {
        conn->write("No project");
    }
    conn->finish();
}

void Server::dumpCompletions(const std::shared_ptr<QueryMessage> &/*query*/, Connection *conn)
{
    if (mCompletionThread) {
        conn->write(mCompletionThread->dump());
    } else {
        conn->write("No completions");
    }
    conn->finish();
}

void Server::dumpCompilationDatabase(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project)
        project = currentProject();
    if (!project) {
        conn->write("No current project");
        conn->finish(1);
        return;
    }
    if (project->state() != Project::Loaded) {
        conn->write("Project loading");
        conn->finish(1);
        return;
    }

    conn->write(project->toCompilationDatabase());
    conn->finish();
}

void Server::suspend(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    const Path p = query->match().pattern();
    enum Mode {
        List,
        All,
        Clear,
        File
    } mode = List;
    if (p == "clear") {
        mode = Clear;
    } else if (p == "all") {
        mode = All;
    } else if (!p.isEmpty()) {
        mode = File;
    }

    std::shared_ptr<Project> project;
    if (mode == File) {
        project = projectForQuery(query);
    } else {
        project = currentProject();
    }
    switch (mode) {
    case All:
        mSuspended = true;
        conn->write("All files are suspended.");
        break;
    case Clear:
        mSuspended = false;
        if (project && project->state() == Project::Loaded)
            project->clearSuspendedFiles();
        conn->write("No files suspended.");
        break;
    case List:
        if (mSuspended)
            conn->write("All files are suspended.");
        if (project && project->state() == Project::Loaded) {
            const Set<uint32_t> suspendedFiles = project->suspendedFiles();
            if (suspendedFiles.isEmpty()) {
                conn->write<512>("No files suspended for project %s", project->path().constData());
            } else {
                for (const auto &it : suspendedFiles)
                    conn->write<512>("%s is suspended", Location::path(it).constData());
            }
        }
        break;
    case File:
        if (!project) {
            conn->write("No project");
        } else if (project->state() != Project::Loaded) {
            conn->write("Project loading");
        } else if (!p.isFile()) {
            conn->write<512>("%s doesn't seem to exist", p.constData());
        } else {
            const uint32_t fileId = Location::fileId(p);
            if (fileId) {
                conn->write<512>("%s is no%s suspended", p.constData(),
                                 project->toggleSuspendFile(fileId) ? "w" : " longer");
            } else {
                conn->write<512>("%s is not indexed", p.constData());
            }
        }
        break;
    }
    conn->finish();
}

void Server::syncProject(const std::shared_ptr<QueryMessage> &/*query*/, Connection *conn)
{
    if (std::shared_ptr<Project> project = currentProject()) {
        if (!project->startSync(Project::Sync_Synchronous))
            project->startSync(Project::Sync_Asynchronous);

    } else {
        conn->write("No active project");
    }
    conn->finish();
}

void Server::handleVisitFileMessage(const std::shared_ptr<VisitFileMessage> &message, Connection *conn)
{
    uint32_t fileId = 0;
    bool visit = false;

    std::shared_ptr<Project> project = mProjects.value(message->project());
    const uint64_t key = message->key();
    if (project && project->isActiveJob(key)) {
        assert(message->file() == message->file().resolved());
        fileId = Location::insertFile(message->file());
        visit = project->visitFile(fileId, message->file(), key);
    }
    VisitFileResponseMessage msg(fileId, visit);
    conn->send(msg);
}

void Server::restoreFileIds()
{
    const Path p = mOptions.dataDir + "fileids";
    DataFile fileIdsFile(mOptions.dataDir + "fileids");
    if (fileIdsFile.open(DataFile::Read)) {
        Hash<Path, uint32_t> pathsToIds;
        fileIdsFile >> pathsToIds;
        Location::init(pathsToIds);
    } else {
        if (!fileIdsFile.error().isEmpty()) {
            error("Can't restore file ids: %s", fileIdsFile.error().constData());
        }
        clearProjects();
    }
}

bool Server::saveFileIds()
{
    const uint32_t lastId = Location::lastId();
    if (mLastFileId == lastId)
        return true;
    DataFile fileIdsFile(mOptions.dataDir + "fileids");
    if (!fileIdsFile.open(DataFile::Write)) {
        error("Can't save file ids: %s", fileIdsFile.error().constData());
        return false;
    }
    const Hash<Path, uint32_t> pathsToIds = Location::pathsToIds();
    fileIdsFile << pathsToIds;
    if (!fileIdsFile.flush()) {
        error("Can't save file ids: %s", fileIdsFile.error().constData());
        return false;
    }

    mLastFileId = lastId;
    return true;
}

void Server::onUnload()
{
    std::shared_ptr<Project> cur = currentProject();
    for (const auto &it : mProjects) {
        if (it.second->state() != Project::Unloaded && it.second != cur && !it.second->isIndexing()) {
            it.second->unload();
        }
    }
}

template <typename T>
static inline bool slowContains(const LinkedList<T> &list, const T &t)
{
    for (T i : list) {
        if (i == t)
            return true;
    }
    return false;
}

void Server::stopServers()
{
    Path::rm(mOptions.socketFile);
    mUnixServer.reset();
}

void Server::codeCompleteAt(const std::shared_ptr<QueryMessage> &query, Connection *conn)
{
    const String q = query->query();
    Deserializer deserializer(q);
    Path path;
    int line, column;
    deserializer >> path >> line >> column;
    path.resolve();
    std::shared_ptr<Project> project = projectForQuery(query);
    if (!project) {
        error("No project found for %s", path.constData());
        conn->finish();
        return;
    }
    const uint32_t fileId = Location::insertFile(path);
    Source source = project->sources(fileId).value(query->buildIndex());
    if (source.isNull()) {
        for (uint32_t dep : project->dependencies(fileId, Project::DependsOnArg)) {
            source = project->sources(dep).value(query->buildIndex());
            if (!source.isNull()) {
                source.fileId = fileId;
                break;
            }
        }

        if (source.isNull()) {
            error("No source found for %s", path.constData());
            conn->finish();
            return;
        }
    }
    if (!mCompletionThread) {
        mCompletionThread = new CompletionThread(mOptions.completionCacheSize);
        mCompletionThread->start();
    }

    const Location loc(fileId, line, column);
    unsigned int flags = CompletionThread::None;
    if (query->type() == QueryMessage::PrepareCodeCompleteAt)
        flags |= CompletionThread::Refresh;
    if (query->flags() & QueryMessage::ElispList)
        flags |= CompletionThread::Elisp;
    if (!(query->flags() & QueryMessage::SynchronousCompletions)) {
        conn->finish();
        conn = 0;
    }
    error() << "Got completion" << String::format("%s:%d:%d", path.constData(), line, column);
    mCompletionThread->completeAt(source, loc, flags, query->unsavedFiles().value(path), conn);
}

void Server::dumpJobs(Connection *conn)
{
    mJobScheduler->dump(conn);
}

class TestConnection : public Connection
{
public:
    TestConnection(const Path &workingDirectory)
        : Connection(), mIsFinished(false), mWorkingDirectory(workingDirectory)
    {}
    virtual bool send(const Message &message)
    {
        if (message.messageId() == Message::FinishMessageId) {
            mIsFinished = true;
            finished()(this, 0);
        } else if (message.messageId() == Message::ResponseId) {
            String response = reinterpret_cast<const ResponseMessage &>(message).data();
            if (response.startsWith(mWorkingDirectory)) {
                response.remove(0, mWorkingDirectory.size());
            }
            mOutput.append(response);
        }
        return true;
    }
    List<String> output() const { return mOutput; }
    bool isFinished() const { return mIsFinished; }
private:
    bool mIsFinished;
    List<String> mOutput;
    const Path mWorkingDirectory;
};

bool Server::runTests()
{
    assert(!mOptions.tests.isEmpty());
    bool ret = true;
    int sourceCount = 0;
    mIndexerMessageReceived.connect([&sourceCount]() {
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
        for (const auto &source : sources) {
            if (!source.isString()) {
                error() << "Invalid source" << source;
                ret = false;
                continue;
            }
            if (!index("clang " + source.convert<String>(), workingDirectory, workingDirectory)) {
                error() << "Failed to index" << ("clang " + source.convert<String>()) << workingDirectory;
                ret = false;
                continue;
            }
            ++sourceCount;
        }
        mOptions.syncThreshold = sourceCount;
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
                const String location = Location::encode(test.operator[]<String>("location"), workingDirectory);
                if (location.isEmpty()) {
                    error() << "Invalid test. Invalid location";
                    ret = false;
                    continue;
                }
                query.reset(new QueryMessage(QueryMessage::FollowLocation));
                query->setQuery(location);
            } else if (type == "references") {
                const String location = Location::encode(test.operator[]<String>("location"), workingDirectory);
                if (location.isEmpty()) {
                    error() << "Invalid test. Invalid location";
                    ret = false;
                    continue;
                }
                query.reset(new QueryMessage(QueryMessage::ReferencesLocation));
                query->setQuery(location);
            } else if (type == "references-name") {
                const String name = test.operator[]<String>("name");
                if (name.isEmpty()) {
                    error() << "Invalid test. Invalid name";
                    ret = false;
                    continue;
                }
                query.reset(new QueryMessage(QueryMessage::ReferencesLocation));
                query->setQuery(name);
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
            handleQueryMessage(query, &conn);
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
