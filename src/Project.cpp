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

#include "Project.h"

#include <fnmatch.h>
#include <memory>
#include <boost/regex.hpp>

#include "Diagnostic.h"
#include "FileManager.h"
#include "CompilerManager.h"
#include "IndexDataMessage.h"
#include "JobScheduler.h"
#include "LogOutputMessage.h"
#include "rct/DataFile.h"
#include "rct/Log.h"
#include "rct/MemoryMonitor.h"
#include "rct/Path.h"
#include "rct/Rct.h"
#include "rct/ReadLocker.h"
#include "rct/Thread.h"
#include "rct/Value.h"
#include "RTags.h"
#include "RTagsLogOutput.h"
#include "Server.h"
#include "RTagsVersion.h"

enum { DirtyTimeout = 100, ReloadCompileCommandsTimeout = 500 };

class Dirty
{
public:
    virtual ~Dirty() {}
    virtual Set<uint32_t> dirtied() const = 0;
    virtual bool isDirty(const SourceList &) = 0;
};

class SimpleDirty : public Dirty
{
public:
    void init(const std::shared_ptr<Project> &project, const Set<uint32_t> &dirty = Set<uint32_t>())
    {
        mProject = project;
        for (auto fileId : dirty) {
            mDirty.insert(fileId);
            mDirty += project->dependencies(fileId, Project::DependsOnArg);
        }
    }

    virtual Set<uint32_t> dirtied() const override
    {
        return mDirty;
    }

    void insert(uint32_t fileId)
    {
        if (mDirty.insert(fileId))
            mDirty += mProject->dependencies(fileId, Project::DependsOnArg);
    }

    virtual bool isDirty(const SourceList &sourceList) override
    {
        return mDirty.contains(sourceList.fileId());
    }

    Set<uint32_t> mDirty;
    std::shared_ptr<Project> mProject;
};

class ComplexDirty : public Dirty
{
public:
    virtual Set<uint32_t> dirtied() const override
    {
        return mDirty;
    }
    void insertDirtyFile(uint32_t fileId)
    {
        mDirty.insert(fileId);
    }
    inline uint64_t lastModified(uint32_t fileId)
    {
        uint64_t &time = mLastModified[fileId];
        if (!time) {
            time = Location::path(fileId).lastModifiedMs();
        }
        return time;
    }

    Hash<uint32_t, uint64_t> mLastModified;
    Set<uint32_t> mDirty;
};

class SuspendedDirty : public ComplexDirty
{
public:
    bool isDirty(const SourceList &) override
    {
        return false;
    }
};

class IfModifiedDirty : public ComplexDirty
{
public:
    IfModifiedDirty(const std::shared_ptr<Project> &project, const Match &match = Match())
        : mProject(project), mMatch(match)
    {
    }

    virtual bool isDirty(const SourceList &sourceList) override
    {
        bool ret = false;

        const uint32_t fileId = sourceList.fileId();
        if (mMatch.isEmpty() || mMatch.match(Location::path(fileId))) {
            for (auto it : mProject->dependencies(fileId, Project::ArgDependsOn)) {
                uint64_t depLastModified = lastModified(it);
                if (!depLastModified || depLastModified > sourceList.parsed) {
                    ret = true;
                    insertDirtyFile(it);
                }
            }
            if (ret)
                mDirty.insert(fileId);

            assert(!ret || mDirty.contains(fileId));
        }
        return ret;
    }

    std::shared_ptr<Project> mProject;
    Match mMatch;
};


class WatcherDirty : public ComplexDirty
{
public:
    WatcherDirty(const std::shared_ptr<Project> &project, const Set<uint32_t> &modified)
    {
        for (auto it : modified) {
            mModified[it] = project->dependencies(it, Project::DependsOnArg);
        }
    }

    virtual bool isDirty(const SourceList &sourceList) override
    {
        bool ret = false;

        for (auto it : mModified) {
            const auto &deps = it.second;
            if (deps.contains(sourceList.fileId())) {
                const uint64_t depLastModified = lastModified(it.first);
                if (!depLastModified || depLastModified > sourceList.parsed) {
                    // dependency is gone
                    ret = true;
                    insertDirtyFile(it.first);
                }
            }
        }

        if (ret)
            insertDirtyFile(sourceList.fileId());
        return ret;
    }

    Hash<uint32_t, Set<uint32_t> > mModified;
};

static bool loadDependencies(DataFile &file, Dependencies &dependencies)
{
    int size;
    file >> size;
    for (int i=0; i<size; ++i) {
        uint32_t fileId;
        file >> fileId;
        if (!fileId)
            return false;
        dependencies[fileId] = new DependencyNode(fileId);
    }
    for (int i=0; i<size; ++i) {
        int links;
        file >> links;
        if (links) {
            uint32_t dependee;
            file >> dependee;
            DependencyNode *ee = dependencies[dependee];
            if (!ee) {
                return false;
            }
            while (links--) {
                uint32_t dependent;
                file >> dependent;
                DependencyNode *ent = dependencies[dependent];
                if (!ent) {
                    return false;
                }
                ent->include(ee);
            }
        }
    }
    return true;
}

static void saveDependencies(DataFile &file, const Dependencies &dependencies)
{
    file << static_cast<int>(dependencies.size());
    for (const auto &it : dependencies) {
        file << it.first;
    }
    for (const auto &it : dependencies) {
        file << static_cast<int>(it.second->dependents.size());
        if (!it.second->dependents.isEmpty()) {
            file << it.first;
            for (const auto &dep : it.second->dependents) {
                file << dep.first;
            }
        }
    }
}

Project::Project(const Path &path)
    : mPath(path), mSourceFilePathBase(RTags::encodeSourceFilePath(Server::instance()->options().dataDir, path)),
      mJobCounter(0), mJobsStarted(0), mBytesWritten(0), mSaveDirty(false)
{
    Path srcPath = mPath;
    RTags::encodePath(srcPath);
    const Server::Options &options = Server::instance()->options();
    const Path tmp = options.dataDir + srcPath;
    mProjectFilePath = tmp + "/project";
    mSourcesFilePath = tmp + "/sources";
}

Project::~Project()
{
    if (mSaveDirty)
        save();
    for (const auto &job : mActiveJobs) {
        assert(job.second);
        Server::instance()->jobScheduler()->abort(job.second);
    }
    mDependencies.deleteAll();

    assert(EventLoop::isMainThread());
    mDirtyTimer.stop();
    mReloadCompileCommandsTimer.stop();
}

static bool hasSourceDependency(const DependencyNode *node, const std::shared_ptr<Project> &project, Set<uint32_t> &seen)
{
    const Path path = Location::path(node->fileId);
    // error("%s %d %d", path.constData(), path.isFile(), path.isSource());
    if (path.isFile() && path.isSource() && project->hasSource(node->fileId)) {
        return true;
    }
    for (auto it : node->dependents) {
        if (seen.insert(it.first) && hasSourceDependency(it.second, project, seen))
            return true;
    }
    return false;
}

static inline bool hasSourceDependency(const DependencyNode *node, const std::shared_ptr<Project> &project)
{
    Set<uint32_t> seen;
    return hasSourceDependency(node, project, seen);
}

bool Project::readSources(const Path &path, IndexParseData &data, String *err)
{
    DataFile file(path, RTags::SourcesFileVersion);
    if (!file.open(DataFile::Read)) {
        Path::rm(path);
        if (err && !file.error().isEmpty())
            *err = file.error();
        return false;
    }

    file >> data;

    if (Sandbox::hasRoot()) {
        forEachSource(data, [](Source &source) {
                for (String &arg : source.arguments) {
                    arg = Sandbox::decoded(arg);
                }
                return Continue;
            });
    }
    return true;
}

bool Project::init()
{
    const JobScheduler::JobScope scope(Server::instance()->jobScheduler());
    const Server::Options &options = Server::instance()->options();
    if (!(options.options & Server::NoFileSystemWatch)) {
        mWatcher.modified().connect(std::bind(&Project::onFileModified, this, std::placeholders::_1));
        mWatcher.added().connect(std::bind(&Project::onFileAdded, this, std::placeholders::_1));
        mWatcher.removed().connect(std::bind(&Project::onFileRemoved, this, std::placeholders::_1));
    }
    if (!(options.options & Server::NoFileManager)) {
        mFileManager.reset(new FileManager(shared_from_this()));
        mWatcher.removed().connect([this](const Path &path) { if (mWatchedPaths.value(path.parentDir()) & Watch_FileManager) mFileManager->onFileRemoved(path); });
        mWatcher.added().connect([this](const Path &path) { if (mWatchedPaths.value(path.parentDir()) & Watch_FileManager) mFileManager->onFileAdded(path); });
    }

    mDirtyTimer.timeout().connect(std::bind(&Project::onDirtyTimeout, this, std::placeholders::_1));
    mReloadCompileCommandsTimer.timeout().connect(std::bind(&Project::reloadCompileCommands, this));

    String err;
    if (!Project::readSources(mSourcesFilePath, mIndexParseData, &err)) {
        if (!err.isEmpty())
            error("Sources restore error %s: %s", mPath.constData(), err.constData());

        return false;
    }

    auto reindexAll = [this]() {
        mProjectFilePath.visit([](const Path &path) {
                if (strcmp(path.fileName(), "sources")) {
                    if (path.isDir()) {
                        Path::rmdir(path);
                    } else {
                        path.rm();
                    }
                }
                return Path::Continue;
            });
        auto parseData = std::move(mIndexParseData);
        processParseData(std::move(parseData));
    };

    DataFile file(mProjectFilePath, RTags::DatabaseVersion);
    if (!file.open(DataFile::Read)) {
        if (!file.error().isEmpty())
            error("Restore error %s: %s", mPath.constData(), file.error().constData());
        reindexAll();
        return true;
    }

    {
        std::lock_guard<std::mutex> lock(mMutex);
        file >> mVisitedFiles;
        Sandbox::decode(mVisitedFiles);
    }
    file >> mDiagnostics;
    for (const auto &info : mIndexParseData.compileCommands)
        watch(Location::path(info.first), Watch_CompileCommands);

    if (!loadDependencies(file, mDependencies)) {
        mDependencies.deleteAll();
        mVisitedFiles.clear();
        mDiagnostics.clear();
        error("Restore error %s: Failed to load dependencies.", mPath.constData());
        reindexAll();
        return true;
    }

    for (const auto &dep : mDependencies) {
        watchFile(dep.first);
    }

    bool needsSave = false;
    std::unique_ptr<ComplexDirty> dirty;

    if (Server::instance()->suspended()) {
        dirty.reset(new SuspendedDirty);
    } else {
        dirty.reset(new IfModifiedDirty(shared_from_this()));
    }

    Set<uint32_t> missingFileMaps;
    {
        List<uint32_t> removed;
        int idx = 0;
        bool outputDirty = false;
        if (mDependencies.size() >= 100) {
            logDirect(LogLevel::Error, String::format<128>("Restoring %s ", mPath.constData()), LogOutput::StdOut);
            outputDirty = true;
        }
        const std::shared_ptr<Project> project = shared_from_this();
        for (auto it : mDependencies) {
            const Path path = Location::path(it.first);
            if (!path.isFile()) {
                warning() << path << "seems to have disappeared";
                dirty.get()->insertDirtyFile(it.first);

                const Set<uint32_t> dependents = dependencies(it.first, DependsOnArg);
                for (auto dependent : dependents) {
                    dirty.get()->insertDirtyFile(dependent);
                }
                removed << it.first;
                needsSave = true;
            } else {
                String errorString;
                if (!validate(it.first,  options.options & Server::ValidateFileMaps ? Validate : StatOnly, &errorString)) {
                    if (!errorString.isEmpty()) {
                        if (outputDirty) {
                            outputDirty = false;
                            logDirect(LogLevel::Error, String("\n"), LogOutput::StdOut);
                        }
                        error() << errorString;
                    }
                    if (hasSource(it.first) || hasSourceDependency(it.second, project)) {
                        missingFileMaps.insert(it.first);
                    } else {
                        removed << it.first;
                        needsSave = true;
                    }
                }
            }
            if (++idx % 100 == 0) {
                outputDirty = true;
                logDirect(LogLevel::Error, ".", 1, LogOutput::StdOut);
                // error("%d/%d (%.2f%%)", idx, count, (idx / static_cast<double>(count)) * 100.0);
            }
        }
        if (outputDirty)
            logDirect(LogLevel::Error, "\n", 1, LogOutput::StdOut);
        for (uint32_t r : removed) {
            removeDependencies(r);
        }
    }

    forEachSourceList([&dirty, this, &needsSave](SourceList &src) -> VisitResult {
            uint32_t fileId = src.fileId();
            const Path sourceFile = Location::path(fileId);
            if (!sourceFile.isFile()) {
                warning() << sourceFile << "seems to have disappeared";
                removeDependencies(fileId);
                dirty.get()->insertDirtyFile(fileId);
                needsSave = true;
                return Remove;
            }
            watchFile(fileId);
            return Continue;
        });

    reloadCompileCommands();

    if (needsSave)
        save();
    startDirtyJobs(dirty.get(), IndexerJob::Dirty|IndexerJob::NoAbort);
    // don't want to abort the jobs we just started from reloadCompileCommands
    if (!missingFileMaps.isEmpty()) {
        SimpleDirty simple;
        simple.init(shared_from_this(), missingFileMaps);
        startDirtyJobs(&simple, IndexerJob::Dirty);
    }
    return true;
}

bool Project::match(const Match &p, bool *indexed) const
{
    Path paths[] = { p.pattern(), p.pattern() };
    paths[1].resolve();
    const int count = paths[1].compare(paths[0]) ? 2 : 1;
    bool ret = false;
    const Path resolvedPath = mPath.resolved();
    for (int i=0; i<count; ++i) {
        const Path &path = paths[i];
        const uint32_t id = Location::fileId(path);
        if (id && isIndexed(id)) {
            if (indexed)
                *indexed = true;
            return true;
        } else if (mFiles.contains(path) || p.match(mPath) || p.match(resolvedPath)) {
            if (!indexed)
                return true;
            ret = true;
        }
    }
    if (indexed)
        *indexed = false;
    return ret;
}

static const char *severities[] = { "none", "warning", "error", "fixit", "note", "skipped" };
static String formatDiagnostics(const Diagnostics &diagnostics, Flags<QueryMessage::Flag> flags, uint32_t fileId = 0)
{
    if (flags & QueryMessage::JSON) {
        std::function<Value(uint32_t, Location, const Diagnostic &)> toValue = [&toValue, flags](uint32_t file, Location loc, const Diagnostic &diagnostic) {
            Value value;
            if (loc.fileId() != file)
                value["file"] = loc.path();
            value["line"] = loc.line();
            value["column"] = loc.column();
            if (diagnostic.length > 0)
                value["length"] = diagnostic.length;
            value["type"] = severities[diagnostic.type];
            if (!diagnostic.message.isEmpty())
                value["message"] = diagnostic.message;
            if (!diagnostic.children.isEmpty()) {
                Value &children = value["children"];
                for (const auto &c : diagnostic.children) {
                    children.push_back(toValue(file, c.first, c.second));
                }
            }
            return value;
        };


        Diagnostics::const_iterator it;
        Diagnostics::const_iterator end;
        if (fileId) {
            it = diagnostics.lower_bound(Location(fileId, 0, 0));
            end = diagnostics.lower_bound(Location(fileId + 1, 0, 0));
        } else {
            it = diagnostics.begin();
            end = diagnostics.end();
        }

        Value val;
        Value &checkStyle = val["checkStyle"];
        Value *currentFile = 0;
        uint32_t lastFileId = 0;
        while (it != diagnostics.end()) {
            if (it->first.fileId() != lastFileId) {
                lastFileId = it->first.fileId();
                if (fileId && lastFileId != fileId)
                    break;
                currentFile = &checkStyle[it->first.path()];
            }

            currentFile->push_back(toValue(lastFileId, it->first, it->second));
            ++it;
        }
        return val.toJSON();
    }

    static const char *header[] = {
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n  <checkstyle>",
        "(list 'checkstyle "
    };
    static const char *fileEmpty[] = {
        "\n    <file name=\"%s\">\n    </file>",
        "(cons \"%s\" nil)"
    };
    static const char *startFile[] = {
        "\n    <file name=\"%s\">",
        "(cons \"%s\" (list"
    };
    static const char *endFile[] = {
        "\n    </file>",
        "))"
    };
    static const char *trailer[] = {
        "\n  </checkstyle>",
        ")"
    };
    std::function<String(Location , const Diagnostic &, uint32_t)> formatDiagnostic;

    enum DiagnosticsFormat {
        Diagnostics_XML,
        Diagnostics_Elisp
    } const format = flags & QueryMessage::Elisp ? Diagnostics_Elisp : Diagnostics_XML;

    if (format == Diagnostics_XML) {
        formatDiagnostic = [&formatDiagnostic](Location loc, const Diagnostic &diagnostic, uint32_t) {
            return String::format<256>("\n      <error line=\"%d\" column=\"%d\" %sseverity=\"%s\" message=\"%s\"/>",
                                       loc.line(), loc.column(),
                                       (diagnostic.length <= 0 ? ""
                                        : String::format<32>("length=\"%d\" ", diagnostic.length).constData()),
                                       severities[diagnostic.type], RTags::xmlEscape(diagnostic.message).constData());
        };
    } else {
        formatDiagnostic = [&formatDiagnostic](Location loc, const Diagnostic &diagnostic, uint32_t file) {
            String children;
            if (!diagnostic.children.isEmpty()) {
                children = "(list";
                for (const auto &c : diagnostic.children) {
                    children << ' ' << formatDiagnostic(c.first, c.second, file);
                }
                children << ")";
            } else {
                children = "nil";
            }
            const bool fn = (loc.fileId() != file);
            return String::format<256>(" (list %s%s%s %d %d %s '%s \"%s\" %s)",
                                       fn ? "\"" : "",
                                       fn ? loc.path().constData() : "nil",
                                       fn ? "\"" : "",
                                       loc.line(),
                                       loc.column(),
                                       diagnostic.length > 0 ? String::number(diagnostic.length).constData() : "nil",
                                       severities[diagnostic.type],
                                       RTags::elispEscape(diagnostic.message).constData(),
                                       children.constData());
        };
    }
    String ret;
    if (fileId) {
        const Path path = Location::path(fileId);
        ret << header[format];

        Diagnostics::const_iterator it = diagnostics.lower_bound(Location(fileId, 0, 0));
        bool found = false;
        while (it != diagnostics.end() && it->first.fileId() == fileId) {
            if (!found) {
                found = true;
                ret << String::format<256>(startFile[format], path.constData());
            }

            ret << formatDiagnostic(it->first, it->second, fileId);
            ++it;
        }
        if (!found) {
            ret << String::format<256>(fileEmpty[format], path.constData());
        }
        ret << endFile[format] << trailer[format];
    } else {
        uint32_t lastFileId = 0;
        bool first = true;
        const Set<uint32_t> active = Server::instance()->activeBuffers();
        for (const auto &entry : diagnostics) {
            Location loc = entry.first;
            if (!active.isEmpty() && !active.contains(loc.fileId()))
                continue;
            const Diagnostic &diagnostic = entry.second;
            if (loc.fileId() != lastFileId) {
                if (first) {
                    ret = header[format];
                    first = false;
                }
                if (lastFileId)
                    ret << endFile[format];
                lastFileId = loc.fileId();
                ret << String::format<256>(startFile[format], loc.path().constData());
            }
            ret << formatDiagnostic(loc, diagnostic, lastFileId);
        }
        if (lastFileId)
            ret << endFile[format];
        if (!first)
            ret << trailer[format];
    }
    return ret;
}

void Project::onJobFinished(const std::shared_ptr<IndexerJob> &job, const std::shared_ptr<IndexDataMessage> &msg)
{
    mBytesWritten += msg->bytesWritten();
    std::shared_ptr<IndexerJob> restart;
    const uint32_t fileId = msg->fileId();
    auto j = mActiveJobs.take(fileId);
    if (!j) {
        error() << "Couldn't find JobData for" << Location::path(fileId) << msg->id() << job->id << job.get();
        return;
    } else if (j != job) {
        error() << "Wrong IndexerJob for" << Location::path(fileId) << msg->id() << job->id << job.get();
        return;
    }

    const bool success = job->flags & IndexerJob::Complete;
    assert(!(job->flags & IndexerJob::Aborted));
    assert(((job->flags & (IndexerJob::Complete|IndexerJob::Crashed)) == IndexerJob::Complete)
           || ((job->flags & (IndexerJob::Complete|IndexerJob::Crashed)) == IndexerJob::Crashed));
    const auto &options = Server::instance()->options();
    if (!success) {
        releaseFileIds(job->visited);
    }

    if (!hasSource(msg->fileId())) {
        releaseFileIds(job->visited);
        error() << "Can't find source for" << Location::path(fileId);
        return;
    }
    if (!(msg->flags() & IndexDataMessage::ParseFailure)) {
        for (uint32_t file : job->visited) {
            if (!validate(file, Validate)) {
                releaseFileIds(job->visited);
                dirty(job->fileId());
                return;
            }
        }
    }

    const int idx = mJobCounter - mActiveJobs.size();
    const Diagnostics changed = updateDiagnostics(msg->diagnostics());
    if (!changed.isEmpty() || options.options & Server::Progress) {
        log([&](const std::shared_ptr<LogOutput> &output) {
                if (output->testLog(RTags::DiagnosticsLevel)) {
                    QueryMessage::Flag format = QueryMessage::XML;
                    if (output->flags() & RTagsLogOutput::Elisp) {
                        // I know this is RTagsLogOutput because it returned
                        // true for testLog(RTags::DiagnosticsLevel)
                        format = QueryMessage::Elisp;
                    }
                    if (!msg->diagnostics().isEmpty()) {
                        const String log = formatDiagnostics(changed, format, false);
                        if (!log.isEmpty()) {
                            output->log(log);
                        }
                    }
                    if (options.options & Server::Progress) {
                        if (format == QueryMessage::XML) {
                            output->vlog("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<progress index=\"%d\" total=\"%d\"></progress>",
                                         idx, mJobCounter);
                        } else {
                            output->vlog("(list 'progress %d %d)", idx, mJobCounter);
                        }
                    }
                }
            });
    }

    Set<uint32_t> visited = msg->visitedFiles();
    updateFixIts(visited, msg->fixIts());
    updateDependencies(msg);
    if (success) {
        forEachSources([&msg](Sources &sources) -> VisitResult {
                // error() << "finished with" << Location::path(msg->fileId()) << sources.contains(msg->fileId()) << msg->parseTime();
                if (sources.contains(msg->fileId())) {
                    sources[msg->fileId()].parsed = msg->parseTime();
                }
                return Continue;
            });
        logDirect(LogLevel::Error, String::format("[%3d%%] %d/%d %s %s. (%s)",
                                                  static_cast<int>(round((double(idx) / double(mJobCounter)) * 100.0)), idx, mJobCounter,
                                                  String::formatTime(time(0), String::Time).constData(),
                                                  msg->message().constData(),
                                                  (job->priority == IndexerJob::HeaderError
                                                   ? "header-error"
                                                   : String::format<16>("priority %d", job->priority).constData())),
                  LogOutput::StdOut|LogOutput::TrailingNewLine);
    } else {
        assert(msg->indexerJobFlags() & IndexerJob::Crashed);
        logDirect(LogLevel::Error, String::format("[%3d%%] %d/%d %s %s indexing crashed.",
                                                  static_cast<int>(round((double(idx) / double(mJobCounter)) * 100.0)), idx, mJobCounter,
                                                  String::formatTime(time(0), String::Time).constData(),
                                                  Location::path(fileId).toTilde().constData()),
                  LogOutput::StdOut|LogOutput::TrailingNewLine);
    }

    if (mActiveJobs.isEmpty()) {
        save();
        double timerElapsed = (mTimer.elapsed() / 1000.0);
        const double averageJobTime = timerElapsed / mJobsStarted;
        const String m = String::format<1024>("Jobs took %.2fs%s. We're using %lldmb of memory. ",
                                              timerElapsed, mJobsStarted > 1 ? String::format(", (avg %.2fs)", averageJobTime).constData() : "",
                                              static_cast<unsigned long long>(MemoryMonitor::usage() / (1024 * 1024)));
        Log(LogLevel::Error, LogOutput::StdOut|LogOutput::TrailingNewLine) << m;
        mJobsStarted = mJobCounter = 0;

        // error() << "Finished this
    } else {
        mSaveDirty = true;
    }
}

void Project::diagnose(uint32_t fileId)
{
    log([&](const std::shared_ptr<LogOutput> &output) {
            if (output->testLog(RTags::DiagnosticsLevel)) {
                QueryMessage::Flag format = QueryMessage::XML;
                if (output->flags() & RTagsLogOutput::Elisp) {
                    // I know this is RTagsLogOutput because it returned
                    // true for testLog(RTags::DiagnosticsLevel)
                    format = QueryMessage::Elisp;
                }
                const String log = formatDiagnostics(mDiagnostics, format, fileId);
                if (!log.isEmpty())
                    output->log(log);
            }
        });
}

void Project::diagnoseAll()
{
    log([&](const std::shared_ptr<LogOutput> &output) {
            if (output->testLog(RTags::DiagnosticsLevel)) {
                QueryMessage::Flag format = QueryMessage::XML;
                if (output->flags() & RTagsLogOutput::Elisp) {
                    // I know this is RTagsLogOutput because it returned
                    // true for testLog(RTags::DiagnosticsLevel)
                    format = QueryMessage::Elisp;
                }
                const String log = formatDiagnostics(mDiagnostics, format);
                if (!log.isEmpty())
                    output->log(log);
            }
        });
}

String Project::diagnosticsToString(Flags<QueryMessage::Flag> flags, uint32_t fileId)
{
    return formatDiagnostics(mDiagnostics, flags, fileId);
}

bool Project::save()
{
    Path::mkdir(mSourcesFilePath.parentDir(), Path::Recursive);
    {
        DataFile file(mSourcesFilePath, RTags::SourcesFileVersion);
        if (!file.open(DataFile::Write)) {
            error("Save error %s: %s", mProjectFilePath.constData(), file.error().constData());
            return false;
        }
        file << mIndexParseData;
    }
    {
        DataFile file(mProjectFilePath, RTags::DatabaseVersion);
        if (!file.open(DataFile::Write)) {
            error("Save error %s: %s", mProjectFilePath.constData(), file.error().constData());
            return false;
        }
        {
            std::lock_guard<std::mutex> lock(mMutex);
            if (Sandbox::hasRoot()) {
                file << Sandbox::encoded(mVisitedFiles);
            } else {
                file << mVisitedFiles;
            }
        }
        file << mDiagnostics;
        saveDependencies(file, mDependencies);
        if (!file.flush()) {
            error("Save error %s: %s", mProjectFilePath.constData(), file.error().constData());
            return false;
        }
    }
    mSaveDirty = false;
    return true;
}

void Project::index(const std::shared_ptr<IndexerJob> &job)
{
    const Path sourceFile = job->sourceFile;
    static const char *fileFilter = getenv("RTAGS_FILE_FILTER");
    if (fileFilter && !strstr(job->sourceFile.constData(), fileFilter)) {
        error() << "Not indexing" << job->sourceFile.constData() << "because of file filter"
                << fileFilter;
        return;
    }

    if (Server::instance()->suspended() && hasSource(job->fileId()) && (job->flags & IndexerJob::Compile)) {
        return;
    }

    std::shared_ptr<IndexerJob> &ref = mActiveJobs[job->fileId()];
    if (ref) {
        // warning() << "Aborting a job" << ref.get() << Location::path(job->fileId());
        releaseFileIds(ref->visited);
        Server::instance()->jobScheduler()->abort(ref);
        --mJobCounter;
    }
    ref = job;

    ++mJobsStarted;
    if (!mJobCounter++) {
        mTimer.start();
    }

    Server::instance()->jobScheduler()->add(job);
}

void Project::onFileModified(const Path &path)
{
    debug() << path << "was modified";
    onFileAddedOrModified(path);
}

void Project::onFileAdded(const Path &path)
{
    debug() << path << "was added";
    onFileAddedOrModified(path);
}

void Project::onFileAddedOrModified(const Path &file)
{
    const uint32_t fileId = Location::fileId(file);
    debug() << file << "was modified" << fileId;
    if (!fileId)
        return;
    // error() << file.fileName() << mCompileCommandsInfos.dir << file;
    if (mIndexParseData.compileCommands.contains(fileId)) {
        mReloadCompileCommandsTimer.restart(ReloadCompileCommandsTimeout, Timer::SingleShot);
        return;
    }

    if (Server::instance()->suspended() || mSuspendedFiles.contains(fileId)) {
        warning() << file << "is suspended. Ignoring modification";
        return;
    }
    Server::instance()->jobScheduler()->clearHeaderError(fileId);
    if (mPendingDirtyFiles.insert(fileId)) {
        mDirtyTimer.restart(DirtyTimeout, Timer::SingleShot);
    }
}

void Project::onFileRemoved(const Path &file)
{
    const uint32_t fileId = Location::fileId(file);
    debug() << file << "was removed" << fileId;
    if (!fileId)
        return;

    if (mIndexParseData.compileCommands.contains(fileId)) {
        reloadCompileCommands();
        return;
    }
    removeSource(fileId);

    Server::instance()->jobScheduler()->clearHeaderError(fileId);

    if (Server::instance()->suspended() || mSuspendedFiles.contains(fileId)) {
        warning() << file << "is suspended. Ignoring modification";
        return;
    }
    if (mPendingDirtyFiles.insert(fileId)) {
        mDirtyTimer.restart(DirtyTimeout, Timer::SingleShot);
    }
}

void Project::onDirtyTimeout(Timer *)
{
    Set<uint32_t> dirtyFiles = std::move(mPendingDirtyFiles);
    WatcherDirty dirty(shared_from_this(), dirtyFiles);
    const int dirtied = startDirtyJobs(&dirty, IndexerJob::Dirty);
    debug() << "onDirtyTimeout" << dirtyFiles << dirtied;
}

SourceList Project::sources(uint32_t fileId) const
{
    SourceList ret;
    forEachSources([&ret, fileId](const Sources &srcs) {
            const auto it = srcs.find(fileId);
            if (it != srcs.end())
                ret += it->second;
            return Continue;
        });
    return ret;
}

bool Project::hasSource(uint32_t fileId) const
{
    bool ret = false;
    forEachSources([&ret, fileId](const Sources &srcs) {
            if (srcs.contains(fileId)) {
                ret = true;
                return Stop;
            }
            return Continue;
        });
    return ret;
}

Set<uint32_t> Project::dependencies(uint32_t fileId, DependencyMode mode) const
{
    Set<uint32_t> ret;
    if (mode == All) {
        for (const auto &node : mDependencies) {
            ret.insert(node.first);
        }
        return ret;
    }
    ret.insert(fileId);
    std::function<void(uint32_t)> fill = [&](uint32_t file) {
        if (DependencyNode *node = mDependencies.value(file)) {
            const auto &nodes = (mode == ArgDependsOn ? node->includes : node->dependents);
            for (const auto &it : nodes) {
                if (ret.insert(it.first))
                    fill(it.first);
            }
        }
    };
    fill(fileId);
    return ret;
}

bool Project::dependsOn(uint32_t source, uint32_t header) const
{
    Set<uint32_t> seen;
    std::function<bool(DependencyNode *node)> dep = [&](DependencyNode *node) {
        assert(node);
        if (!seen.insert(node->fileId))
            return false;
        if (node->dependents.contains(source))
            return true;
        for (const std::pair<uint32_t, DependencyNode*> &n : node->dependents) {
            if (dep(n.second))
                return true;
        }
        return false;
    };
    DependencyNode *node = mDependencies.value(header);
    return node && dep(node);
}

void Project::removeDependencies(uint32_t fileId)
{
    if (DependencyNode *node = mDependencies.take(fileId)) {
        for (auto it : node->includes)
            it.second->dependents.remove(fileId);
        for (auto it : node->dependents)
            it.second->includes.remove(fileId);
        delete node;
    }
}

void Project::updateDependencies(const std::shared_ptr<IndexDataMessage> &msg)
{
    const bool prune = !(msg->flags() & (IndexDataMessage::InclusionError|IndexDataMessage::ParseFailure));
    Set<uint32_t> files;
    for (auto pair : msg->files()) {
        assert(pair.first);
        DependencyNode *&node = mDependencies[pair.first];
        if (!node) {
            node = new DependencyNode(pair.first);
            if (pair.second & IndexDataMessage::Visited)
                files.insert(pair.first);
        } else if (pair.second & IndexDataMessage::Visited) {
            files.insert(pair.first);
            if (prune) {
                for (auto it : node->includes)
                    it.second->dependents.remove(pair.first);
                node->includes.clear();
            }
        }
        watchFile(pair.first);
    }

    // // ### this probably deletes and recreates the same nodes very very often
    for (auto it : msg->includes()) {
        assert(it.first);
        assert(it.second);
        DependencyNode *&includer = mDependencies[it.first];
        DependencyNode *&inclusiary = mDependencies[it.second];
        files.insert(it.first);
        files.insert(it.second);
        if (!includer)
            includer = new DependencyNode(it.first);
        if (!inclusiary)
            inclusiary = new DependencyNode(it.second);
        includer->include(inclusiary);
    }
}

int Project::reindex(const Match &match,
                     const std::shared_ptr<QueryMessage> &query,
                     const std::shared_ptr<Connection> &wait)
{
    if (query->type() == QueryMessage::Reindex) {
        Set<uint32_t> dirtyFiles;

        const auto end = mDependencies.constEnd();
        for (auto it = mDependencies.constBegin(); it != end; ++it) {
            if (!dirtyFiles.contains(it->first) && (match.isEmpty() || match.match(Location::path(it->first)))) {
                dirtyFiles.insert(it->first);
            }
        }
        if (dirtyFiles.isEmpty())
            return 0;
        SimpleDirty dirty;
        dirty.init(shared_from_this(), dirtyFiles);
        return startDirtyJobs(&dirty, IndexerJob::Reindex, query->unsavedFiles(), wait);
    } else {
        assert(query->type() == QueryMessage::CheckReindex);
        IfModifiedDirty dirty(shared_from_this(), match);
        return startDirtyJobs(&dirty, IndexerJob::Dirty, query->unsavedFiles(), wait);
    }
}

int Project::remove(const Match &match)
{
    int count = 0;
    forEachSourceList([&match, &count](SourceList &src) -> VisitResult {
            if (match.match(Location::path(src.fileId()))) {
                ++count;
                return Remove;
            }
            return Continue;
        });
    return count;
}

int Project::startDirtyJobs(Dirty *dirty, Flags<IndexerJob::Flag> flags,
                            const UnsavedFiles &unsavedFiles,
                            const std::shared_ptr<Connection> &wait)
{
    const JobScheduler::JobScope scope(Server::instance()->jobScheduler());
    Set<uint32_t> toIndex;
    forEachSourceList([dirty, &toIndex](const SourceList &sourceList) -> VisitResult {
            if (dirty->isDirty(sourceList))
                toIndex.insert(sourceList.fileId());
            return Continue;
        });
    const Set<uint32_t> dirtyFiles = dirty->dirtied();

    {
        std::lock_guard<std::mutex> lock(mMutex);
        for (const auto &fileId : dirtyFiles) {
            mVisitedFiles.remove(fileId);
        }
    }

    const bool noAbort = flags & IndexerJob::NoAbort;
    flags &= ~IndexerJob::NoAbort;
    assert(flags == IndexerJob::Dirty || flags == IndexerJob::Reindex);

    std::weak_ptr<Connection> weakConn = wait;
    for (uint32_t fileId : toIndex) {
        if (noAbort) {
            assert(!wait); // this can't happen now, if it could we would have to call finish
            if (mActiveJobs.contains(fileId))
                continue;
        }

        auto job = std::make_shared<IndexerJob>(sources(fileId), flags, shared_from_this(), unsavedFiles);
        if (wait) {
            job->destroyed.connect([weakConn](IndexerJob *) {
                    // should arguably be refcounted but I don't know if anyone waits for multiple jobs
                    if (auto strong = weakConn.lock()) {
                        strong->finish();
                    }
                });
        }
        index(job);
    }

    return toIndex.size();
}

bool Project::isIndexed(uint32_t fileId) const
{
    return mDependencies.contains(fileId);
}

const Set<uint32_t> &Project::suspendedFiles() const
{
    return mSuspendedFiles;
}

void Project::clearSuspendedFiles()
{
    mSuspendedFiles.clear();
}

bool Project::toggleSuspendFile(uint32_t file)
{
    if (!mSuspendedFiles.insert(file)) {
        mSuspendedFiles.remove(file);
        return false;
    }
    return true;
}

void Project::setSuspended(uint32_t file, bool suspended)
{
    if (suspended) {
        mSuspendedFiles.insert(file);
    } else {
        mSuspendedFiles.remove(file);
    }
}

bool Project::isSuspended(uint32_t file) const
{
    return mSuspendedFiles.contains(file);
}

void Project::updateFixIts(const Set<uint32_t> &visited, FixIts &fixIts)
{
    for (auto v : visited) {
        const auto fit = fixIts.find(v);
        if (fit == fixIts.end()) {
            mFixIts.erase(v);
        } else {
            mFixIts[v] = fit->second;
        }
    }
}

Diagnostics Project::updateDiagnostics(const Diagnostics &diagnostics)
{
    Diagnostics ret;
    uint32_t lastFile = 0;
    for (const auto &it : diagnostics) {
        const uint32_t f = it.first.fileId();
        if (f != lastFile) {
            Diagnostics::iterator old = mDiagnostics.lower_bound(Location(f, 0, 0));
            bool found = false;
            while (old != mDiagnostics.end() && old->first.fileId() == f) {
                found = true;
                mDiagnostics.erase(old++);
            }
            lastFile = f;

            if (it.second.isNull() && !found) {
                continue;
            }
        }
        if (!it.second.isNull())
            mDiagnostics.insert(it);
        ret.insert(it);
    }
    return ret;
}

String Project::fixIts(uint32_t fileId) const
{
    const auto it = mFixIts.find(fileId);
    String out;
    if (it != mFixIts.end()) {
        const Set<FixIt> &fixIts = it->second;
        if (!fixIts.isEmpty()) {
            auto f = fixIts.end();
            do {
                --f;
                if (!out.isEmpty())
                    out.append('\n');
                out.append(String::format<32>("%d:%d %d %s", f->line, f->column, f->length, f->text.constData()));

            } while (f != fixIts.begin());
        }
    }
    return out;
}

void Project::findSymbols(const String &unencoded,
                          const std::function<void(SymbolMatchType, const String &, const Set<Location> &)> &inserter,
                          Flags<QueryMessage::Flag> queryFlags,
                          uint32_t fileFilter)
{
    const String string = Sandbox::encoded(unencoded);
    const bool wildcard = queryFlags & QueryMessage::WildcardSymbolNames && (string.contains('*') || string.contains('?'));
    const bool caseInsensitive = queryFlags & QueryMessage::MatchCaseInsensitive;
    const String::CaseSensitivity cs = caseInsensitive ? String::CaseInsensitive : String::CaseSensitive;
    String lowerBound;
    if (wildcard) {
        if (!caseInsensitive) {
            const size_t size = string.size();
            for (size_t i=0; i<size; ++i) {
                if (string.at(i) == '?' || string.at(i) == '*') {
                    lowerBound = string.left(i);
                    break;
                }
            }
        }
    } else if (!caseInsensitive) {
        lowerBound = string;
    }

    auto processFile = [this, &lowerBound, &string, wildcard, cs, &inserter](uint32_t file) {
        auto symNames = openSymbolNames(file);
        if (!symNames)
            return;
        const int count = symNames->count();
        // error() << "Looking at" << count << Location::path(dep.first)
        //         << lowerBound << string;
        uint32_t idx = 0;
        if (!lowerBound.isEmpty()) {
            idx = symNames->lowerBound(lowerBound);
            if (idx == std::numeric_limits<uint32_t>::max()) {
                return;
            }
        }

        for (int i=idx; i<count; ++i) {
            const String entry = symNames->keyAt(i);
            // error() << i << count << entry;
            SymbolMatchType type = Exact;
            if (!string.isEmpty()) {
                if (wildcard) {
                    if (!Rct::wildCmp(string.constData(), entry.constData(), cs)) {
                        continue;
                    }
                    type = Wildcard;
                } else if (!entry.startsWith(string, cs)) {
                    if (cs == String::CaseInsensitive) {
                        continue;
                    } else {
                        break;
                    }
                    type = StartsWith;
                } else if (entry.size() != string.size()) {
                    type = StartsWith;
                }
            }
            inserter(type, entry, symNames->valueAt(i));
        }
    };

    if (fileFilter) {
        processFile(fileFilter);
    } else {
        for (const auto &dep : mDependencies) {
            processFile(dep.first);
        }
    }
}

List<RTags::SortedSymbol> Project::sort(const Set<Symbol> &symbols, Flags<QueryMessage::Flag> flags)
{
    List<RTags::SortedSymbol> sorted;
    sorted.reserve(symbols.size());
    for (const Symbol &symbol : symbols) {
        RTags::SortedSymbol node(symbol.location);
        if (!symbol.isNull()) {
            node.isDefinition = symbol.isDefinition();
            if (flags & QueryMessage::DeclarationOnly && node.isDefinition) {
                const Symbol decl = findTarget(symbol);
                if (!decl.isNull() && !decl.isDefinition()) {
                    assert(decl.usr == symbol.usr);
                    continue;
                }
            } else if (flags & QueryMessage::DefinitionOnly && !node.isDefinition) {
                continue;
            }
            node.kind = symbol.kind;
        }
        sorted.push_back(node);
    }

    if (flags & QueryMessage::ReverseSort) {
        std::sort(sorted.begin(), sorted.end(), std::greater<RTags::SortedSymbol>());
    } else {
        std::sort(sorted.begin(), sorted.end());
    }
    return sorted;
}

void Project::watch(const Path &dir, WatchMode mode)
{
    if (!dir.isEmpty()) {
        const auto opts = Server::instance()->options().options;
        if (opts & Server::WatchSourcesOnly && mode != Watch_SourceFile)
            return;
        const auto it = mWatchedPaths.find(dir);
        if (it != mWatchedPaths.end()) {
            it->second |= mode;
            return;
        }
        if (opts & Server::WatchSystemPaths || !dir.isSystem()) {
            auto &m = mWatchedPaths[dir];
            if (!m)
                mWatcher.watch(dir);
            m |= mode;
        }
    }
}

void Project::watchFile(uint32_t fileId)
{
    const WatchMode mode = hasSource(fileId) ? Watch_SourceFile : Watch_Dependency;
    watch(Location::path(fileId).parentDir(), mode);
}

void Project::clearWatch(Flags<WatchMode> mode)
{
    auto it = mWatchedPaths.begin();
    while (it != mWatchedPaths.end()) {
        it->second &= ~mode;
        if (!it->second) {
            mWatcher.unwatch(it->first);
            mWatchedPaths.erase(it++);
        } else {
            ++it;
        }
    }
}

void Project::unwatch(const Path &dir, WatchMode mode)
{
    auto it = mWatchedPaths.find(dir);
    if (it == mWatchedPaths.end()) {
        it = mWatchedPaths.find(dir.resolved());
        if (it == mWatchedPaths.end()) {
            const auto opts = Server::instance()->options().options;
            if (!(opts & Server::WatchSourcesOnly) || mode != Watch_Dependency)
                error() << "We're not watching this directory" << dir;
            return;
        }
    }
    if (!(it->second &= ~mode)) {
        mWatcher.unwatch(it->first);
        mWatchedPaths.erase(it);
    }
}

String Project::toCompileCommands() const
{
    const Flags<Source::CommandLineFlag> flags = (Source::IncludeCompiler
                                                  | Source::IncludeSourceFile
                                                  | Source::IncludeDefines
                                                  | Source::IncludeIncludePaths
                                                  | Source::QuoteDefines
                                                  | Source::FilterBlacklist);
    Value ret;
    forEachSource([&ret, flags](const Source &source) -> VisitResult {
            Value unit;
            unit["directory"] = source.directory;
            unit["file"] = source.sourceFile();
            unit["command"] = String::join(source.toCommandLine(flags), " ").constData();
            ret.push_back(unit);
            return Continue;
        });

    return ret.toJSON(true);
}

Symbol Project::findSymbol(Location location, int *index)
{
    if (index)
        *index = -1;
    if (location.isNull())
        return Symbol();
    auto symbols = openSymbols(location.fileId());
    if (!symbols || !symbols->count())
        return Symbol();

    bool exact = false;
    uint32_t idx = symbols->lowerBound(location, &exact);
    if (exact) {
        if (index)
            *index = idx;
        return symbols->valueAt(idx);
    }
    switch (idx) {
    case 0:
        return Symbol();
    case std::numeric_limits<uint32_t>::max():
        idx = symbols->count() - 1;
        break;
    default:
        --idx;
        break;
    }

    const Symbol &ret = symbols->valueAt(idx);
    if (ret.location.fileId() != location.fileId()
        || ret.location.line() != location.line()
        || (location.column() - ret.location.column() >= ret.symbolLength)) {
        return Symbol();
    }
    if (index)
        *index = idx;
    return ret;
}

Set<Symbol> Project::findTargets(const Symbol &symbol)
{
    Set<Symbol> ret;
    if (symbol.isNull() || symbol.flags & Symbol::ImplicitDestruction)
        return ret;
    auto sameKind = [&symbol](CXCursorKind kind) {
        if (kind == symbol.kind)
            return true;
        if (symbol.isClass())
            return kind == CXCursor_ClassDecl || kind == CXCursor_StructDecl;
        return false;
    };

    switch (symbol.kind) {
    case CXCursor_ClassDecl:
    case CXCursor_ClassTemplate:
    case CXCursor_StructDecl:
        if (symbol.isDefinition() && !(symbol.flags & Symbol::TemplateSpecialization))
            return ret;
    case CXCursor_FunctionDecl:
    case CXCursor_CXXMethod:
    case CXCursor_Destructor:
    case CXCursor_Constructor:
    case CXCursor_FieldDecl:
    case CXCursor_VarDecl:
    case CXCursor_FunctionTemplate: {
        const Set<Symbol> symbols = findByUsr(symbol.usr, symbol.location.fileId(),
                                              symbol.isDefinition() ? ArgDependsOn : DependsOnArg, symbol.location);
        for (const auto &c : symbols) {
            if (sameKind(c.kind) && symbol.isDefinition() != c.isDefinition()) {
                ret.insert(c);
                break;
            }
        }

        if (!ret.isEmpty() || (symbol.kind != CXCursor_VarDecl && symbol.kind != CXCursor_FieldDecl))
            break; }
        // fall through
    default:
        if (symbol.flags & Symbol::TemplateReference) {
            for (const String &usr : findTargetUsrs(symbol)) {
                ret.unite(findByUsr(usr, symbol.location.fileId(), Project::DependsOnArg));
            }
        } else {
            for (const String &usr : findTargetUsrs(symbol.location)) {
                ret.unite(findByUsr(usr, symbol.location.fileId(), Project::ArgDependsOn));
            }
        }
        break;
    }

    return ret;
}

Set<Symbol> Project::findByUsr(const String &usr, uint32_t fileId, DependencyMode mode, Location filtered)
{
    assert(fileId);
    Set<Symbol> ret;
    String tusr = Sandbox::encoded(usr);
    for (uint32_t file : dependencies(fileId, mode)) {
        auto usrs = openUsrs(file);
        // error() << usrs << Location::path(file) << usr;
        if (usrs) {
            // SBROOT
            for (Location loc : usrs->value(tusr)) {
                // error() << "got a loc" << loc;
                const Symbol c = findSymbol(loc);
                if (!c.isNull())
                    ret.insert(c);
            }
            // for (int i=0; i<usrs->count(); ++i) {
            //     error() << i << usrs->count() << usrs->keyAt(i) << usrs->valueAt(i);
            // }
        }
    }
    if (ret.isEmpty() || (!filtered.isNull() && ret.size() == 1 && ret.begin()->location == filtered)) {
        for (const auto &dep : mDependencies) {
            auto usrs = openUsrs(dep.first);
            if (usrs) {
                // SBROOT
                tusr = Sandbox::encoded(usr);
                for (Location loc : usrs->value(tusr)) {
                    const Symbol c = findSymbol(loc);
                    if (!c.isNull())
                        ret.insert(c);
                }
            }
        }
    }

    if (ret.isEmpty() && usr.startsWith("/")) { // for break statements and includes
        Symbol sym;
        sym.location = Location::fromPathLineAndColumn(usr);
        ret.insert(sym);
        return ret;
    }

    return ret;
}

static Set<Symbol> findReferences(const Set<Symbol> &inputs,
                                  const std::shared_ptr<Project> &project,
                                  std::function<bool(const Symbol &, const Symbol &)> filter)
{
    Set<Symbol> ret;
    // const bool isClazz = s.isClass();
    for (const Symbol &input : inputs) {
        //warning() << "Calling findReferences" << input.location;
        auto process = [&](uint32_t dep) {
            // error() << "Looking at file" << Location::path(dep) << "for input" << input.location;
            auto targets = project->openTargets(dep);
            if (targets) {
                // SBROOT
                const String tusr = Sandbox::encoded(input.usr);
                const Set<Location> locations = targets->value(tusr);
                // error() << "Got locations for usr" << input.usr << locations;
                for (const auto &loc : locations) {
                    auto sym = project->findSymbol(loc);
                    if (filter(input, sym))
                        ret.insert(sym);
                }
            }
        };
        const Set<uint32_t> deps = project->dependencies(input.location.fileId(), Project::DependsOnArg);
        for (auto dep : deps)
            process(dep);

        if (ret.isEmpty()) {
            for (auto dep : project->dependencies()) {
                if (!deps.contains(dep.first))
                    process(dep.first);
            }
        }
    }
    return ret;
}

static Set<Symbol> findReferences(const Symbol &in,
                                  const std::shared_ptr<Project> &project,
                                  std::function<bool(const Symbol &, const Symbol &)> filter,
                                  Set<Symbol> *inputsPtr = 0)
{
    Set<Symbol> inputs;
    Symbol s;
    Location location;
    if (in.isReference()) {
        const Symbol target = project->findTarget(in);
        if (!target.isNull()) {
            if (target.kind != CXCursor_MacroExpansion) {
                s = target;
            } else {
                s = in;
                auto usrs = project->findTargetUsrs(s.location);
                if (!usrs.isEmpty())
                    s.usr = *usrs.begin();
                // error() << "GOT USRS" << usrs;
                s.location = location = target.location;
            }
        }
    }
    if (s.isNull())
        s = in;
    if (location.isNull())
        location = s.location;

    // error() << "findReferences" << s.location << in.location << s.kind;
    switch (s.kind) {
    case CXCursor_CXXMethod:
        if (s.flags & Symbol::VirtualMethod) {
            inputs = project->findVirtuals(s);
            break;
        }
        // fall through
    case CXCursor_FunctionTemplate:
    case CXCursor_FunctionDecl:
    case CXCursor_ClassTemplate:
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
    case CXCursor_UnionDecl:
    case CXCursor_VarDecl:
    case CXCursor_TypedefDecl:
    case CXCursor_Namespace:
    case CXCursor_Constructor:
    case CXCursor_Destructor:
    case CXCursor_ConversionFunction:
    case CXCursor_NamespaceAlias:
        inputs = project->findByUsr(s.usr, location.fileId(),
                                    s.isDefinition() ? Project::ArgDependsOn : Project::DependsOnArg,
                                    in.location);
        break;
    default:
        inputs.insert(s);
        break;
    case CXCursor_FirstInvalid:
        return Set<Symbol>();
    }
    if (inputsPtr)
        *inputsPtr = inputs;
    return findReferences(inputs, project, filter);
}

Set<Symbol> Project::findCallers(const Symbol &symbol)
{
    const bool isClazz = symbol.isClass();
    return ::findReferences(symbol, shared_from_this(), [isClazz](const Symbol &input, const Symbol &ref) {
            if (isClazz && (ref.isConstructorOrDestructor() || ref.kind == CXCursor_CallExpr))
                return false;
            if (ref.isReference()
                || (input.kind == CXCursor_Constructor && (ref.kind == CXCursor_VarDecl || ref.kind == CXCursor_FieldDecl))) {
                return true;
            }
            if (input.kind == CXCursor_ClassTemplate && ref.flags & Symbol::TemplateSpecialization) {
                return true;
            }
            return false;
        });
}

Set<Symbol> Project::findAllReferences(const Symbol &symbol)
{
    if (symbol.isNull())
        return Set<Symbol>();

    Set<Symbol> inputs;
    inputs.insert(symbol);
    inputs.unite(findByUsr(symbol.usr, symbol.location.fileId(), DependsOnArg, symbol.location));
    Set<Symbol> ret = inputs;
    for (const auto &input : inputs) {
        Set<Symbol> inputLocations;
        ret.unite(::findReferences(input, shared_from_this(), [](const Symbol &, const Symbol &) {
                    return true;
                }, &inputLocations));
        ret.unite(inputLocations);
    }
    return ret;
}

Set<Symbol> Project::findVirtuals(const Symbol &symbol)
{
    if (symbol.kind != CXCursor_CXXMethod || !(symbol.flags & Symbol::VirtualMethod))
        return Set<Symbol>();

    Symbol parent = [this](const Symbol &sym) {
        for (const String &usr : findTargetUsrs(sym.location)) {
            const Set<Symbol> syms = findByUsr(usr, sym.location.fileId(), ArgDependsOn);
            for (const Symbol &s : syms) {
                if (findTargetUsrs(s.location).isEmpty()) {
                    return s;
                }
            }
        }
        return sym;
    }(symbol);

    assert(!parent.isNull());
    if (parent.isDefinition()) {
        const auto target = findTarget(parent);
        if (!target.isNull()) {
            parent = target;
        }
    }

    Set<Symbol> symSet;
    symSet.insert(parent);
    Set<Symbol> ret = ::findReferences(symSet, shared_from_this(), [](const Symbol &, const Symbol &ref) {
            // error() << "considering" << ref.location << ref.kindSpelling();
            if (ref.kind == CXCursor_CXXMethod) {
                return true;
            }
            return false;
        });
    ret.insert(parent);
    const Symbol target = findTarget(parent);
    if (!target.isNull())
        ret.insert(target);
    return ret;
}

Set<String> Project::findTargetUsrs(Location loc)
{
    Set<String> usrs;
    auto targets = openTargets(loc.fileId());
    if (targets) {
        const int count = targets->count();
        for (int i=0; i<count; ++i) {
            if (targets->valueAt(i).contains(loc)) {
                // SBROOT
                usrs.insert(Sandbox::decoded(targets->keyAt(i)));
            }
        }
    }
    return usrs;
}

Set<String> Project::findTargetUsrs(const Symbol &symbol)
{
    if (!(symbol.flags & Symbol::TemplateReference)) {
        return findTargetUsrs(symbol.location);
    }

    Set<String> usrs;
    for (uint32_t fileId : dependencies(symbol.location.fileId(), DependsOnArg)) {
        auto targets = openTargets(fileId);
        if (targets) {
            const int count = targets->count();
            for (int i=0; i<count; ++i) {
                if (targets->valueAt(i).contains(symbol.location)) {
                    // SBROOT
                    usrs.insert(Sandbox::decoded(targets->keyAt(i)));
                }
            }
        }
    }
    return usrs;
}

Set<Symbol> Project::findSubclasses(const Symbol &symbol)
{
    assert(symbol.isClass() && symbol.isDefinition());
    Set<Symbol> ret;
    for (uint32_t dep : dependencies(symbol.location.fileId(), DependsOnArg)) {
        auto symbols = openSymbols(dep);
        if (symbols) {
            const int count = symbols->count();
            for (int i=0; i<count; ++i) {
                const Symbol s = symbols->valueAt(i);
                if (s.baseClasses.contains(symbol.usr))
                    ret.insert(s);
            }
        }
    }
    return ret;
}

void Project::beginScope()
{
    assert(!mFileMapScope);
    mFileMapScope.reset(new FileMapScope(shared_from_this(), Server::instance()->options().maxFileMapScopeCacheSize));
}

void Project::endScope()
{
    assert(mFileMapScope);
    mFileMapScope.reset();
}

static String addDeps(const Dependencies &deps)
{
    if (deps.isEmpty())
        return "nil";
    String ret;
    ret << "(list";
    for (const auto &dep : deps) {
        ret << " \"" << Location::path(dep.first) << "\"";
    }
    ret << ")";
    return ret;
}

String Project::dumpDependencies(uint32_t fileId, const List<String> &args, Flags<QueryMessage::Flag> flags) const
{
    String ret;

    auto dumpRaw = [&ret, flags](DependencyNode *n) {
        if (!(flags & QueryMessage::Elisp)) {
            ret << Location::path(n->fileId) << "\n";
            for (const auto &inc : n->includes) {
                ret << "  " << Location::path(inc.second->fileId) << "\n";
            }
            for (const auto &dep : n->dependents) {
                ret << "    " << Location::path(dep.second->fileId) << "\n";
            }
            return;
        }

        ret << " (cons \"" << Location::path(n->fileId) << "\" (cons " << addDeps(n->includes) << ' ' << addDeps(n->dependents) << "))\n";
    };

    if (fileId) {
        DependencyNode *node = mDependencies.value(fileId);
        if (!node)
            return String::format<128>("Can't find node for %s", Location::path(fileId).constData());

        if (!node->includes.isEmpty() && (args.isEmpty() || args.contains("includes"))) {
            if (args.size() != 1)
                ret += String::format<256>("  %s includes:\n", Location::path(fileId).constData());
            for (const auto &include : node->includes) {
                ret += String::format<256>("    %s\n", Location::path(include.first).constData());
            }
        }
        if (!node->dependents.isEmpty() && (args.isEmpty() || args.contains("included-by"))) {
            if (args.size() != 1)
                ret += String::format<256>("  %s is included by:\n", Location::path(fileId).constData());
            for (const auto &include : node->dependents) {
                ret += String::format<256>("    %s\n", Location::path(include.first).constData());
            }
        }

        if (args.isEmpty() || args.contains("depends-on")) {
            bool first = args.size() != 1;
            for (auto dep : dependencies(fileId, Project::ArgDependsOn)) {
                if (dep == fileId)
                    continue;
                if (first) {
                    first = false;
                    ret += String::format<256>("  %s depends on:\n", Location::path(fileId).constData());
                }
                ret += String::format<256>("    %s\n", Location::path(dep).constData());
            }
        }

        if (args.isEmpty() || args.contains("depended-on")) {
            bool first = args.size() != 1;
            for (auto dep : dependencies(fileId, Project::DependsOnArg)) {
                if (dep == fileId)
                    continue;
                if (first) {
                    first = false;
                    ret += String::format<256>("  %s is depended on by:\n", Location::path(fileId).constData());
                }
                ret += String::format<256>("    %s\n", Location::path(dep).constData());
            }
        }

        if (args.isEmpty() || args.contains("tree-depends-on")) {
            Set<DependencyNode*> seen;

            DependencyNode *depNode = mDependencies.value(fileId);
            if (depNode) {
                int startDepth = 1;
                if (args.size() != 1) {
                    ++startDepth;
                    ret += String::format<256>("  %s include tree:\n", Location::path(fileId).constData());
                }

                std::function<void(DependencyNode *, int)> process = [&](DependencyNode *n, int depth) {
                    ret += String::format<256>("%s%s", String(depth * 2, ' ').constData(), Location::path(n->fileId).constData());

                    if (seen.insert(n) && !n->includes.isEmpty()) {
                        ret += " includes:\n";
                        for (const auto &includeNode : n->includes) {
                            process(includeNode.second, depth + 1);
                        }
                    } else {
                        ret += '\n';
                    }
                };
                process(depNode, startDepth);
            }
        }
        if (args.size() == 1 && args.contains("raw")) {
            Set<DependencyNode*> all;
            std::function<void(DependencyNode *node)> add = [&](DependencyNode *depNode) {
                assert(depNode);
                if (!all.insert(depNode))
                    return;
                for (const std::pair<uint32_t, DependencyNode*> &n : depNode->includes) {
                    add(n.second);
                }
            };
            add(node);
            ret << "(list\n";
            for (DependencyNode *n : all) {
                dumpRaw(n);
            }
            ret.chop(1);
            ret << ")\n";
        }
    } else {
        ret << "(list\n";
        for (const auto &node : mDependencies) {
            dumpRaw(node.second);
        }
        ret.chop(1);
        ret << ")\n";
    }

    return ret;
}

void Project::dirty(uint32_t fileId)
{
    SimpleDirty dirty;
    Set<uint32_t> dirtyFiles;
    dirtyFiles.insert(fileId);
    dirty.init(shared_from_this(), dirtyFiles);
    startDirtyJobs(&dirty, IndexerJob::Dirty);
}

bool Project::validate(uint32_t fileId, ValidateMode mode, String *err) const
{
    if (mode == Validate) {
        Path path;
        String error;
        const uint32_t opts = fileMapOptions();
        {
            path = sourceFilePath(fileId, fileMapName(SymbolNames));
            FileMap<String, Set<Location> > fileMap;
            if (!fileMap.load(path, opts, &error))
                goto error;
        }
        {
            path = sourceFilePath(fileId, fileMapName(Symbols));
            FileMap<Location, Symbol> fileMap;
            if (!fileMap.load(path, opts, &error))
                goto error;
        }
        {
            path = sourceFilePath(fileId, fileMapName(Targets));
            FileMap<String, Set<Location> > fileMap;
            if (!fileMap.load(path, opts, &error))
                goto error;
        }
        {
            path = sourceFilePath(fileId, fileMapName(Usrs));
            FileMap<String, Set<Location> > fileMap;
            if (!fileMap.load(path, opts, &error))
                goto error;
        }
        return true;
  error:
        if (err)
            Log(err) << "Error during validation:" << Location::path(fileId) << error << path;
        return false;
    } else {
        assert(mode == StatOnly);
        for (auto type : { Symbols, SymbolNames, Targets, Usrs }) {
            const Path p = sourceFilePath(fileId, fileMapName(type));
            if (!p.isFile()) {
                Log(err) << "Error during validation:" << Location::path(fileId) << p << "doesn't exist";
                return false;
            }
        }
    }
    return true;
}

template <typename T>
static inline String toString(const T &t, size_t &max)
{
    String ret;
    Log(&ret, LogOutput::NoTypename) << t;
    max = std::max(max, ret.size());
    ret.replace('\n', ' ');
    return ret;
}

template <>
inline String toString(const String &str, size_t &max)
{
    max = std::max(max, str.size());
    String ret = str;
    ret.replace('\n', ' ');
    return ret;
}

static inline void fixString(String &string, size_t size)
{
    if (string.size() != size)
        string.append(String(size - string.size(), ' '));
}

static List<String> split(const String &value, size_t max)
{
    List<String> words = value.split(' ', String::KeepSeparators);
    List<String> ret(1);
    size_t i = 0;
    while (i < words.size()) {
        const String &word = words.at(i);
        if (ret.last().size() && ret.last().size() + word.size() > max) {
            fixString(ret.last(), max);
            ret.append(String());
            continue;
        }

        if (word.size() > max) {
            assert(ret.last().isEmpty());
            for (size_t j=0; j<word.size(); j += max) {
                if (j)
                    ret.append(String());
                ret.last() = word.mid(j, max);
                fixString(ret.last(), max);
            }
        } else {
            ret.last().append(word);
        }
        ++i;
    }

    fixString(ret.last(), max);
    return ret;
}

template <typename T> struct PreferComma { enum { value = 0 }; };
template <typename T> struct PreferComma<Set<T> > { enum { value = 1 }; };

template <typename T>
static List<String> formatField(const String &value, size_t max)
{
    List<String> ret;
    if (value.size() <= max) {
        ret << value.padded(String::End, max);
    } else {
        if (PreferComma<T>::value)
            ret = value.split(',', String::KeepSeparators);

        if (ret.size() > 1) {
            for (size_t i=0; i<ret.size(); ++i) {
                if (ret.at(i).size() > max) {
                    auto split = ::split(ret.at(i), max);
                    ret.remove(i, 1);
                    ret.insert(i, split);
                    i += split.size() - 1;
                } else if (ret.at(i).size() != max) {
                    ret[i].append(String(max - ret.at(i).size(), ' '));
                }
            }
        } else {
            ret = split(value, max);
        }
    }
    return ret;
}
template <typename Key, typename Value>
static String formatTable(const String &name, const std::shared_ptr<FileMap<Key, Value> > &fileMap, size_t width)
{
    width -= 7; // padding
    List<String> keys, values;
    const int count = fileMap->count();
    size_t maxKey = 0;
    size_t maxValue = 0;
    for (int i=0; i<count; ++i) {
        keys << toString(fileMap->keyAt(i), maxKey);
        values << toString(fileMap->valueAt(i), maxValue);
    }
    if (maxKey + maxValue > width) {
        if (maxKey < maxValue) {
            maxKey = std::min(maxKey, static_cast<size_t>(width * .4));
            maxValue = std::min(maxValue, width - maxKey);
        } else {
            maxValue = std::min(maxValue, static_cast<size_t>(width * .4));
            maxKey = std::min(maxKey, width - maxValue);

        }
    }

    String ret;
    ret.reserve((count + 3) * (maxKey + maxValue + 7));
    ret << name << '\n' << String(name.size(), '-') << '\n';
    const String keyFill(maxKey, ' ');
    const String valueFill(maxValue, ' ');
    for (int i=0; i<count; ++i) {
        const List<String> key = formatField<Key>(keys.at(i), maxKey);
        const List<String> value = formatField<Value>(values.at(i), maxValue);
        const int c = std::max(key.size(), value.size());
        char ch = '|';
        for (int j=0; j<c; ++j) {
            ret << ch << ' ' << key.value(j, keyFill) << ' ' << ch << ' ' << value.value(j, valueFill) << ' ' << ch << '\n';
            ch = '*';
        }
    }
    return ret;
}

void Project::dumpFileMaps(const std::shared_ptr<QueryMessage> &msg, const std::shared_ptr<Connection> &conn)
{
    beginScope();
    String err;

    Path path;
    List<String> args;
    Deserializer deserializer(msg->query());
    deserializer >> path >> args;
    const uint32_t fileId = Location::fileId(path);
    assert(fileId);
    assert(isIndexed(fileId));

    if (args.empty() || args.contains("symbols")) {
        if (auto tbl = openSymbols(fileId, &err)) {
            conn->write(formatTable("Symbols:", tbl, msg->terminalWidth()));
        } else {
            conn->write(err);
        }
    }

    if (args.empty() || args.contains("symbolnames")) {
        if (auto tbl = openSymbolNames(fileId, &err)) {
            conn->write(formatTable("Symbol names:", tbl, msg->terminalWidth()));
        } else {
            conn->write(err);
        }
    }

    if (args.empty() || args.contains("targets")) {
        if (auto tbl = openTargets(fileId, &err)) {
            conn->write(formatTable("Targets:", tbl, msg->terminalWidth()));
        } else {
            conn->write(err);
        }
    }

    if (args.empty() || args.contains("usrs")) {
        if (auto tbl = openUsrs(fileId, &err)) {
            conn->write(formatTable("Usrs:", tbl, msg->terminalWidth()));
        } else {
            conn->write(err);
        }
    }

    if (args.empty() || args.contains("tokens")) {
        if (auto tbl = openTokens(fileId, &err)) {
            conn->write(formatTable("Tokens:", tbl, msg->terminalWidth()));
        } else {
            conn->write(err);
        }
    }


    endScope();
}

void Project::prepare(uint32_t fileId)
{
    if (fileId && isIndexed(fileId)) {
        beginScope();
        String err;
        openSymbolNames(fileId, &err);
        openSymbols(fileId, &err);
        openTargets(fileId, &err);
        openUsrs(fileId, &err);
        debug() << "Prepared" << Location::path(fileId);
        endScope();
    }
}

template <typename T>
size_t estimateMemory(const T &t);
size_t estimateMemory(const String &string);
size_t estimateMemory(const Source::Define &define);
size_t estimateMemory(const Source::Include &include);
size_t estimateMemory(const Source &source);
template <typename T>
size_t estimateMemory(const std::shared_ptr<T> &ptr);
template <typename T>
size_t estimateMemory(const T *t);
template <typename T>
size_t estimateMemory(const Set<T> &container);
template <typename T>
size_t estimateMemory(const List<T> &container);
template <typename Key, typename Value>
size_t estimateMemory(const Map<Key, Value> &container);
template <typename Key, typename Value>
size_t estimateMemory(const Hash<Key, Value> &container);

// impl
template <typename T>
size_t estimateMemory(const T &t)
{
    return sizeof(t);
}

size_t estimateMemory(const String &string)
{
    return string.size() + 1 + sizeof(string) + (sizeof(size_t) + sizeof(size_t));
}

size_t estimateMemory(const Source::Define &define)
{
    return estimateMemory(define.define) + estimateMemory(define.value);
}

size_t estimateMemory(const Source::Include &include)
{
    return estimateMemory(include.type) + estimateMemory(include.path);
}

size_t estimateMemory(const Source &source)
{
    size_t ret = sizeof(Source);
    ret += estimateMemory(source.extraCompiler);
    ret += estimateMemory(source.defines);
    ret += estimateMemory(source.includePaths);
    ret += estimateMemory(source.directory);
    return ret;
}

template <typename T>
size_t estimateMemory(const std::shared_ptr<T> &ptr)
{
    size_t ret = sizeof(ptr) + sizeof(size_t);
    if (ptr) {
        ret += estimateMemory(*ptr);
    } else {
        ret += sizeof(T);
    }
    return ret;
}

template <typename T>
size_t estimateMemory(const T *t)
{
    if (t)
        return estimateMemory(*t);
    return 0;
}

template <typename T>
size_t estimateMemory(const Set<T> &container)
{
    size_t ret = sizeof(Set<T>) + sizeof(void*);
    for (const T &value : container) {
        ret += estimateMemory(value) + (sizeof(void*) * 2); // might not be entirely fair to std::vector
    }
    return ret;
}

template <typename T>
size_t estimateMemory(const List<T> &container)
{
    size_t ret = sizeof(List<T>) + sizeof(void*);
    for (const T &value : container) {
        ret += estimateMemory(value);
    }
    return ret;
}

template <typename T>
size_t estimateKeyValueContainer(const T &t)
{
    size_t ret = sizeof(T) + sizeof(void*);
    for (const auto &pair : t) {
        ret += estimateMemory(pair.first) + estimateMemory(pair.second) + (sizeof(void*) * 2);
    }
    return ret;
}

template <typename Key, typename Value>
size_t estimateMemory(const Map<Key, Value> &container)
{
    return estimateKeyValueContainer(container);
}

template <typename Key, typename Value>
size_t estimateMemory(const Hash<Key, Value> &container)
{
    return estimateKeyValueContainer(container);
}

String Project::estimateMemory() const
{
    List<String> ret;
    size_t total = 0;
    auto add = [&ret, &total](const char *name, size_t size) {
        total += size;
        ret << String::format<128>("%s: %.2fmb", name, size / (1024.0 * 1024.0));
    };
    add("Paths", ::estimateMemory(mFiles));
    add("Visited files", ::estimateMemory(mVisitedFiles));
    add("Diagnostics", ::estimateMemory(mDiagnostics));
    add("Active jobs", ::estimateMemory(mActiveJobs));
    add("Fixits", ::estimateMemory(mFixIts));
    add("Pending dirty files", ::estimateMemory(mPendingDirtyFiles));
    add("Sources", ::estimateMemory(mIndexParseData.sources));
    add("Suspended files", ::estimateMemory(mSuspendedFiles));
    size_t deps = ::estimateMemory(mDependencies);
    for (const auto &dep : mDependencies) {
        deps += ::estimateMemory(*dep.second);
    }
    add("Dependencies", deps);
    add("Total", total);
    return String::join(ret, "\n");
}

void Project::reloadCompileCommands()
{
    mReloadCompileCommandsTimer.stop();
    if (!Server::instance()->suspended()) {
        SourceCache cache;
        Hash<uint32_t, uint32_t> removed;
        IndexParseData data;
        data.project = mPath;
        data.environment = mIndexParseData.environment;
        bool found = false;
        auto it = mIndexParseData.compileCommands.begin();
        while (it != mIndexParseData.compileCommands.end()) {
            const Path file = Location::path(it->first);
            const uint64_t lastModified = file.lastModifiedMs();
            if (!lastModified) {
                for (auto src : it->second.sources) {
                    removed[src.first] = it->first;
                }
                mIndexParseData.compileCommands.erase(it++);
                continue;
            }

            if (lastModified != it->second.lastModifiedMs
                && Server::instance()->loadCompileCommands(data, file, it->second.environment, &cache)) {
                found = true;
            }
            ++it;
        }
        removeSources(removed);
        if (found)
            processParseData(std::move(data));
    }
}

uint32_t Project::fileMapOptions() const
{
    uint32_t options = FileMap<int, int>::None;
    if (Server::instance()->options().options & Server::NoFileLock)
        options |= FileMap<int, int>::NoLock;
    return options;
}

void Project::fixPCH(Source &source)
{
    for (Source::Include &inc : source.includePaths) {
        if (inc.type == Source::Include::Type_FileInclude && inc.isPch()) {
            const uint32_t fileId = Location::insertFile(inc.path);
            inc.path = RTags::encodeSourceFilePath(Server::instance()->options().dataDir, mPath, fileId) + "pch.h";
            error() << "PREPARING" << inc.path;
        }
    }
}

void Project::includeCompletions(Flags<QueryMessage::Flag> flags, const std::shared_ptr<Connection> &conn, Source &&source) const
{
    CompilerManager::applyToSource(source, CompilerManager::IncludeIncludePaths);
    source.includePaths.append(Server::instance()->options().includePaths);
    source.includePaths.sort();
    Set<Path> seen;
    if (flags & QueryMessage::Elisp) {
        conn->write("(list");
    }
    for (const Source::Include &inc : source.includePaths) {
        Path root;
        switch (inc.type) {
        case Source::Include::Type_Framework:
        case Source::Include::Type_SystemFramework:
            root = inc.path.ensureTrailingSlash() + "Headers/";
            break;
        default:
            root = inc.path.ensureTrailingSlash();
            break;
        }
        if (!seen.insert(root))
            continue;
        int depth = 0;
        int maxDepth = Server::instance()->options().maxIncludeCompletionDepth;
        if (!maxDepth)
            maxDepth = INT_MAX;
        std::function<Path::VisitResult(const Path &)> visitor = [&depth, flags, &conn, &visitor, &root, maxDepth](const Path &path) {
            if (path.isHeader()) {
                const String p = path.mid(root.size());
                if (flags & QueryMessage::Elisp) {
                    conn->write<1024>(" \"%s\"", p.constData());
                } else {
                    conn->write(p);
                }
            } else if (depth < maxDepth && path.isDir()) {
                ++depth;
                path.visit(visitor);
                --depth;
            }

            return Path::Continue;
        };
        root.visit(visitor);
    }
    if (flags & QueryMessage::Elisp)
        conn->write(")");
}

Source Project::source(uint32_t fileId, int buildIndex) const
{
    Source ret;
    forEachSources([fileId, &buildIndex, &ret](const Sources &sources) {
            auto it = sources.find(fileId);
            if (it != sources.end()) {
                for (const auto &src : it->second) {
                    if (!buildIndex--) {
                        ret = src;
                        return Stop;
                    }
                }
            }
            return Continue;
        });
    return ret;
}

void Project::reindex(uint32_t fileId, Flags<IndexerJob::Flag> flags)
{
    index(std::make_shared<IndexerJob>(sources(fileId), flags, shared_from_this()));
}

void Project::processParseData(IndexParseData &&data)
{
    Set<uint32_t> index;
    Hash<uint32_t, uint32_t> removed;
    if (mIndexParseData.isEmpty()) {
        mIndexParseData = std::move(data);
        forEachSources([this, &index](const Sources &sources) -> VisitResult {
                for (auto pair : sources) {
                    index.insert(pair.first);
                }
                return Continue;
            });
    } else {
        forEachSource(data.sources, [this, &index](const Source &source) -> VisitResult {
                // only allowing one "loose" build per fileId
                auto &ref = mIndexParseData.sources[source.fileId];
                if (Server::instance()->options().options & Server::AllowMultipleSources) {
                    if (!ref.contains(source)) {
                        ref.append(source);
                        ref.parsed = 0; // dirty
                        if (!(Server::instance()->options().options & Server::NoFileSystemWatch))
                            index.insert(source.fileId);
                    }
                } else {
                    if (ref.isEmpty()) {
                        index.insert(source.fileId);
                        ref.push_back(source);
                    } else if (ref[0] != source) {
                        if (!ref[0].compareArguments(source)) {
                            if (!(Server::instance()->options().options & Server::NoFileSystemWatch))
                                index.insert(source.fileId);
                            ref.parsed = 0; // dirty
                        }
                        ref[0] = source;
                    }
                }
                return Continue;
            });

        for (auto &cc : data.compileCommands) {
            Sources oldSources;
            auto oldIt = mIndexParseData.compileCommands.find(cc.first);
            if (oldIt != mIndexParseData.compileCommands.end()) {
                oldSources = std::move(oldIt->second.sources);
            }

            mIndexParseData.compileCommands[cc.first] = std::move(cc.second);
            forEachSourceList(mIndexParseData.compileCommands[cc.first].sources, [&oldSources, &index](SourceList &list) {
                    const uint32_t fileId = list.fileId();
                    auto oit = oldSources.find(fileId);
                    if (oit != oldSources.end()) {
                        bool same;
                        const auto &oitSources = oit->second;
                        if (list.size() == oitSources.size()) {
                            same = true;
                            for (size_t idx=0; idx<list.size(); ++idx) {
                                if (!oitSources.at(idx).compareArguments(list.at(idx))) {
                                    same = false;
                                    break;
                                }
                            }
                        } else {
                            same = false;
                        }
                        if (same) {
                            list.parsed = oit->second.parsed; // don't want to reparse these, maintain parseTime
                        } else if (!(Server::instance()->options().options & Server::NoFileSystemWatch)) {
                            index.insert(fileId);
                        }
                        oldSources.erase(oit);
                    } else {
                        index.insert(fileId);
                    }
                    return Continue;
                });

            for (auto it : oldSources) {
                removed[it.first] = cc.first;
            }
        }
    }
    removeSources(removed);

    for (uint32_t fileId : index) {
        reindex(fileId, IndexerJob::Compile);
    }
}

void Project::forEachSourceList(const IndexParseData &data, std::function<VisitResult(const SourceList &)> cb)
{
    bool done = false;
    forEachSources(data, [&done, &cb](const Sources &sources) {
            forEachSourceList(sources, [&done, &cb](const SourceList &list) {
                    auto ret = cb(list);
                    assert(ret != Remove);
                    if (ret == Stop)
                        done = true;
                    return ret;
                });
            return done ? Stop : Continue;
        });
}

void Project::forEachSourceList(IndexParseData &data, std::function<VisitResult(SourceList &)> cb)
{
    bool done = false;
    forEachSources(data, [&done, &cb](Sources &sources) {
            forEachSourceList(sources, [&done, &cb](SourceList &list) {
                    auto ret = cb(list);
                    if (ret == Stop)
                        done = true;
                    return ret;
                });
            return done ? Stop : Continue;
        });
}

void Project::forEachSourceList(Sources &sources, std::function<VisitResult(SourceList &source)> cb)
{
    auto it = sources.begin();
    while (it != sources.end()) {
        const auto ret = cb(it->second);
        if (ret == Remove) {
            sources.erase(it++);
        } else if (ret == Stop) {
            break;
        } else {
            ++it;
        }
    }
}

void Project::forEachSourceList(const Sources &sources, std::function<VisitResult(const SourceList &sourceList)> cb)
{
    auto it = sources.begin();
    while (it != sources.end()) {
        const auto ret = cb(it->second);
        assert(ret != Remove);
        if (ret == Stop) {
            break;
        } else {
            ++it;
        }
    }
}

void Project::forEachSource(const Sources &sources, std::function<VisitResult(const Source &source)> cb)
{
    for (auto &src : sources) {
        for (const Source &s : src.second) {
            auto ret = cb(s);
            assert(ret != Remove);
            if (ret == Stop)
                return;
        }
    }
}

void Project::forEachSource(Sources &sources, std::function<VisitResult(Source &source)> cb)
{
    Sources::iterator sit = sources.begin();
    while (sit != sources.end()) {
        SourceList &list = sit->second;
        SourceList::iterator it = list.begin();
        bool done = false;
        while (it != list.end()) {
            const auto ret = cb(*it);
            if (ret == Remove) {
                it = list.erase(it);
            } else if (ret == Stop) {
                done = true;
                break;
            } else {
                ++it;
            }
        }
        if (list.isEmpty()) {
            sources.erase(sit++);
        } else {
            ++sit;
        }
        if (done)
            break;
    }
}

void Project::forEachSources(const IndexParseData &data, std::function<VisitResult(const Sources &sources)> cb)
{
    for (const auto &compileCommands : data.compileCommands) {
        const auto ret = cb(compileCommands.second.sources);
        assert(ret != Remove);
        if (ret == Stop)
            return;
    }
    const auto ret = cb(data.sources);
    (void)ret;
    assert(ret != Remove);
}

void Project::forEachSources(IndexParseData &data, std::function<VisitResult(Sources &sources)> cb)
{
    auto it = data.compileCommands.begin();
    while (it != data.compileCommands.end()) {
        const auto ret = cb(it->second.sources);
        if (ret == Remove) {
            data.compileCommands.erase(it++);
        } else if (ret == Stop) {
            return;
        } else {
            ++it;
        }
    }
    if (cb(data.sources) == Remove)
        data.sources.clear();
}

void Project::forEachSource(const IndexParseData &data, std::function<VisitResult(const Source &source)> cb)
{
    bool stop = false;
    forEachSources(data, [&stop, &cb](const Sources &sources) {
            forEachSource(sources, [&stop, &cb](const Source &src) {
                    const auto ret = cb(src);
                    assert(ret != Remove);
                    if (ret == Stop)
                        stop = true;
                    return ret;
                });
            return stop ? Stop : Continue;
        });
}

void Project::forEachSource(IndexParseData &data, std::function<VisitResult(Source &source)> cb)
{
    bool stop = false;
    forEachSources(data, [&stop, &cb](Sources &sources) {
            forEachSource(sources, [&stop, &cb](Source &src) {
                    const auto ret = cb(src);
                    if (ret == Stop)
                        stop = true;
                    return ret;
                });
            return stop ? Stop : Continue;
        });
}

void Project::removeSources(const Hash<uint32_t, uint32_t> &removed)
{
    for (auto it : removed) {
        if (!hasSource(it.first)) {
            if (it.second)
                error() << Location::path(it.first) << "is no longer in" << Location::path(it.second) << "removing";
            removeSource(it.first);
        }
    }
}

void Project::removeSource(uint32_t fileId)
{
    std::shared_ptr<IndexerJob> job = mActiveJobs.take(fileId);
    if (job) {
        releaseFileIds(job->visited);
        Server::instance()->jobScheduler()->abort(job);
    }
    removeDependencies(fileId);
    Path::rmdir(sourceFilePath(fileId));
}

void Project::validateAll()
{
    SimpleDirty dirty;
    dirty.init(shared_from_this());
    bool clean = true;
    for (auto dep : mDependencies) {
        String err;
        if (!validate(dep.first, Validate, &err)) {
            clean = false;
            dirty.insert(dep.first);
            error() << err;
        }
    }
    if (!clean)
        startDirtyJobs(&dirty, IndexerJob::Dirty);
}
