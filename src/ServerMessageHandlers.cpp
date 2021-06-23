#include "Server.h"

#include "ClangThread.h"
#include "FileManager.h"
#include "Preprocessor.h"

#include "IndexDataMessage.h"
#include "IndexMessage.h"
#include "LogOutputMessage.h"
#include "LogOutputMessage.h"
#include "QueryMessage.h"
#include "VisitFileMessage.h"
#include "VisitFileResponseMessage.h"
#include "rct/FinishMessage.h"
#include "rct/QuitMessage.h"
#include "rct/ResponseMessage.h"

#include "ClassHierarchyJob.h"
#include "CompletionThread.h"
#include "DependenciesJob.h"
#include "FindFileJob.h"
#include "FindSymbolsJob.h"
#include "FollowLocationJob.h"
#include "IncludeFileJob.h"
#include "IncludePathJob.h"
#include "IndexParseData.h"
#include "JobScheduler.h"
#include "ListSymbolsJob.h"
#include "Project.h"
#include "QueryJob.h"
#include "RTagsLogOutput.h"
#include "ReferencesJob.h"
#include "StatusJob.h"
#include "SymbolInfoJob.h"
#include "TokensJob.h"

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

void Server::handleIndexMessage(const std::shared_ptr<IndexMessage> &message, const std::shared_ptr<Connection> &conn)
{
    const Path path = message->compileCommands();
    IndexParseData data;
    data.project = message->projectRoot();
    bool ret = true;
    if (!path.empty()) {
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
            if (arguments.empty()) {
                conn->write("Can't guess args from arguments");
                ret = false;
            }
        }
        if (ret)
            ret = parse(data, std::move(arguments), message->workingDirectory(), 0);
    }
    if (conn)
        conn->finish(ret ? 0 : 1);
    if (ret) {
        auto proj = addProject(data.project.ensureTrailingSlash(), data.compileCommandsFileId);
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
        for (const auto &projects : mProjects) {
            for (const auto &proj : projects.second) {
                if (proj != project) {
                    for (const Path &projectPath : { proj->path(), proj->path().resolved() }) {
                        if (path.startsWith(projectPath)) {
                            FollowLocationJob job(loc, query, proj);
                            if (job.run(conn)) {
                                conn->finish();
                                return;
                            }
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
            for (const auto &proj : it.second) {
                if (proj->isIndexing()) {
                    conn->write("1");
                    conn->finish();
                    return;
                }
            }
        }
    }
    conn->write("0");
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

    std::shared_ptr<Project> project = findProject(fileId);;
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
    if (!query->query().empty()) {
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
        compile.push_back(source.sourceFile().fileName());

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
    if (!fileId && !path.empty()) {
        conn->write<256>("%s is not indexed", query->query().constData());
        conn->finish();
        return;
    }

    std::shared_ptr<Project> project;
    if (fileId) {
        project = findProject(fileId);
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
            if (!out.empty())
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

static size_t sharedRoot(const String &a, const String &b)
{
    const size_t size = std::min(a.size(), b.size());
    assert(size);
    size_t i;
    for (i=0; i<size; ++i) {
        if (a[i] != b[i])
            break;
    }
    return i;
}

static List<std::shared_ptr<Project>> sorted(List<std::shared_ptr<Project>> projects, List<String> *names)
{
    std::sort(projects.begin(), projects.end(), [](const std::shared_ptr<Project> &a, const std::shared_ptr<Project> &b) {
        if (!a->compileCommandsFileId() != !b->compileCommandsFileId()) {
            return !a->compileCommandsFileId();
        }
        assert(a->compileCommandsFileId());
        return Location::path(a->compileCommandsFileId()) < Location::path(b->compileCommandsFileId());
    });
    if (names) {
        names->reserve(projects.size());
        names->append(Location::path(projects[0]->compileCommandsFileId()).parentDir());
        if (projects.size() > 1) {
            size_t sharedRootLength = std::numeric_limits<size_t>::max();
            size_t startIndex = 0;
            if (names->last().isEmpty()) {
                ++startIndex;
                names->append(Location::path(projects[1]->compileCommandsFileId()).parentDir());
            }

            for (size_t i=startIndex + 1; i<projects.size(); ++i) {
                String name = Location::path(projects[i]->compileCommandsFileId()).parentDir();
                names->append(name);
                sharedRootLength = std::min(sharedRootLength, sharedRoot(names->at(startIndex), name));
            }
            if (sharedRootLength) {
                for (size_t i=startIndex; i<names->size(); ++i) {
                    (*names)[i].remove(0, sharedRootLength);
                }
            }
        }
    }
    return projects;
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
    } else if (query->query().empty()) {
        const std::shared_ptr<Project> current = currentProject();
        size_t idx = 0;
        for (const auto &projects : mProjects) {
            List<String> names;
            auto sortedProjects = sorted(projects.second, &names);
            for (size_t i=0; i<sortedProjects.size(); ++i) {
                const auto &proj = sortedProjects[i];
                if (sortedProjects.size() > 1) {
                    String name = names[i];
                    if (name.isEmpty())
                        name = "none";
                    conn->write<128>("%zu: %s (%s)%s", idx++, proj->path().constData(), name.constData(), proj == current ? " <=" : "");
                } else {
                    conn->write<128>("%zu: %s%s", idx++, proj->path().constData(), proj == current ? " <=" : "");
                }
            }
        }
    } else {
        std::shared_ptr<Project> selected;
        bool error = false;
        const Match match = query->match();
        const auto it = mProjects.find(match.pattern());
        bool ok = false;
        unsigned long long index = query->query().toULongLong(&ok);
        if (it != mProjects.end()) {
            selected = it->second.front();
        } else {
            if (ok) {
                for (auto pit = mProjects.begin(); pit != mProjects.end(); ++pit) {
                    const auto projs = sorted(pit->second, nullptr);
                    if (index < projs.size()) {
                        selected = projs[index];
                        break;
                    } else {
                        index -= projs.size();
                    }
                }
            }
            if (!selected) {
                for (const auto &pit : mProjects) {
                    for (const auto &pp : pit.second) {
                        if (pp->match(match)) {
                            if (error) {
                                conn->write(pit.first);
                            } else if (selected) {
                                error = true;
                                conn->write<128>("Multiple matches for %s", match.pattern().constData());
                                conn->write(selected->path());
                                conn->write(pit.first);
                                selected.reset();
                            } else {
                                selected = pp;
                            }
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
    if (q.empty()) {
        conn->write<128>("Running with %zu jobs", mOptions.jobCount);
    } else {
        int jobCount = -1;
        bool ok = false;
        if (q == "default") {
            ok = true;
            jobCount = mDefaultJobCount;
        } else if (q == "pop") {
            if (mJobCountStack.empty()) {
                conn->write<128>("Job count stack is empty");
            } else {
                jobCount = mJobCountStack.back();
                mJobCountStack.pop_back();
                ok = true;
            }
        } else if (q.startsWith("push:")) {
            jobCount = q.mid(5).toLongLong(&ok);
            if (ok && jobCount > 0 && jobCount < MaxJobCount) {
                mJobCountStack.push_back(mOptions.jobCount);
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
                        if (!out.empty()) {
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
                if (sources.empty() && path.isHeader()) {
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
        assert(modeString.empty());
    } else if (p == "all") {
        mode = All;
        assert(modeString.empty());
    } else if (!p.empty()) {
        if (modeString == "on") {
            mode = FileOn;
        } else if (modeString == "off") {
            mode = FileOff;
        } else {
            mode = FileToggle;
        }
    }

    List<Match> matches;
    if (!query->currentFile().empty())
        matches.push_back(query->currentFile());
    if (mode == FileOn || mode == FileOff || mode == FileToggle)
        matches.push_back(p);
    std::shared_ptr<Project> project;
    if (matches.empty()) {
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
            if (suspendedFiles.empty()) {
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
    if (encoded.empty()) {
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
    } else if (!str.empty()) {
        mOptions.debugLocations << str;
    }
    if (mOptions.debugLocations.empty()) {
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

    std::shared_ptr<Project> project = findProject(fileId);
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

    const uint32_t id = message->sourceFileId();
    for (const auto &project : mProjects.value(message->project())) {
        if (project->isActiveJob(id)) {
            assert(message->file() == message->file().resolved());
            fileId = Location::insertFile(message->file());
            visit = project->visitFile(fileId, id);
            break;
        }
    }
    VisitFileResponseMessage msg(fileId, visit);
    conn->send(msg);
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
