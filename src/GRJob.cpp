#include "GRParser.h"
#include "GRJob.h"
#include "Server.h"

GRJob::GRJob(const Path &path)
    : mPath(path), mBatch(0)
{
    if (!mPath.endsWith('/'))
        mPath.append('/');

}

void GRJob::run()
{
    {
        ScopedDB db = Server::instance()->db(Server::GRFiles, Server::Erase, mPath);
        Batch batch(db);
        mBatch = &batch;
        mPath.visit(&GRJob::visit, this);
    }
    finished()(mDirectories);
}

enum FilterResult {
    File,
    C,
    CPlusPlus,
    Directory,
    Filtered
};

static inline FilterResult filter(const Path &path, Path::Type type, int maxSymLinks)
{
    switch (type) {
    case Path::SymLink: {
        if (!--maxSymLinks)
            return Filtered;
        bool ok;
        const Path link = path.followLink(&ok);
        if (!ok)
            return Filtered;
        return filter(link, link.type(), maxSymLinks); }
    case Path::Directory: {
        const int lastSlash = path.lastIndexOf('/', path.size() - 2);
        if (lastSlash > 4 && !strncmp("/.git/", path.constData() + lastSlash - 5, 5))
            return Filtered;
        return Directory; }
    default:
        break;
    }
    const char *extension = path.extension();
    if (extension) {
        if (!strcmp(extension, "o")) {
            return Filtered;
        } else if (!strcmp(extension, "obj")) {
            return Filtered;
        } else if (!strcmp(extension, "lo")) {
            return Filtered;
        } else if (!strcmp(extension, "c")) {
            return C;
        } else if (path.isSource()) {
            return CPlusPlus;
        }
    }
    return File;
}

Path::VisitResult GRJob::visit(const Path &path, void *userData)
{
    const Path::Type type = path.type();
    const FilterResult result = filter(path, type, 10);
    GRJob *recurseJob = reinterpret_cast<GRJob*>(userData);
    switch (result) {
    case Filtered:
        return Path::Continue;
    case Directory:
        recurseJob->mDirectories.append(path);
        return Path::Recurse;
    case C:
    case CPlusPlus: {
        // GRParser parser;
        // Timer timer;
        // const int count = parser.parse(*recurseJob->mDB, path, result == C ? GRParser::None : GRParser::CPlusPlus);
        // error("Parsed %s, %d entries in %dms", path.constData(), count, timer.elapsed());
        break; }
    case File:
        break;
    }
    const Path chopped = path.mid(recurseJob->mPath.size());
    recurseJob->mBatch->add(chopped, true);
    return Path::Continue;
}
