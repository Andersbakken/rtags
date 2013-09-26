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

#include "IndexerJobEsprima.h"
#include "JSParser.h"
#include "Project.h"
#include "RTagsPlugin.h"

class EsprimaPlugin : public RTagsPlugin
{
public:
    virtual std::shared_ptr<IndexerJob> createJob(IndexType type,
                                                  const std::shared_ptr<Project> &project,
                                                  const SourceInformation &sourceInformation)
    {
        if (sourceInformation.isJS())
            return std::shared_ptr<IndexerJobEsprima>(new IndexerJobEsprima(type, project, sourceInformation));
        return std::shared_ptr<IndexerJob>();
    }
    virtual std::shared_ptr<IndexerJob> createJob(const QueryMessage &msg,
                                                  const std::shared_ptr<Project> &project,
                                                  const SourceInformation &sourceInformation)
    {
        if (sourceInformation.isJS())
            return std::shared_ptr<IndexerJob>(new IndexerJobEsprima(msg, project, sourceInformation));
        return std::shared_ptr<IndexerJob>();
    }
};

extern "C" {
RTagsPlugin *createInstance()
{
    return new EsprimaPlugin;
}
};

IndexerJobEsprima::IndexerJobEsprima(IndexType type, const std::shared_ptr<Project> &project,
                                     const SourceInformation &sourceInformation)
    : IndexerJob(type, project, sourceInformation), mState(Pending)
{}

IndexerJobEsprima::IndexerJobEsprima(const QueryMessage &msg,
                                     const std::shared_ptr<Project> &project,
                                     const SourceInformation &sourceInformation)
    : IndexerJob(msg, project, sourceInformation), mState(Pending)
{
}

bool IndexerJobEsprima::abort()
{
    std::unique_lock<std::mutex> lock(mMutex);
    if (mState == Pending)
        return false;
    mState = Aborted;
    return true;
}

void IndexerJobEsprima::start()
{
    std::shared_ptr<IndexerJobEsprima> j = std::static_pointer_cast<IndexerJobEsprima>(shared_from_this());
    ThreadPool::instance()->start(j);
}

void IndexerJobEsprima::run()
{
    assert(mState == Running);
    {
        std::unique_lock<std::mutex> lock(mMutex);
        mState = Running;
    }

    StopWatch timer;
    JSParser parser;
    if (!parser.init()) {
        error() << "Can't init JSParser for" << sourceInformation.sourceFile();
        return;
    }
    if (isAborted())
        return;
    mData.reset(new IndexData(type));
    mData->fileId = sourceInformation.fileId;
    mData->parseTime = time(0);
    if (!parser.parse(sourceInformation.sourceFile(), &mData->symbols, &mData->symbolNames,
                      type == Dump ? 0 : &mData->dependencies, type == Dump ? &mData->logOutput : 0)) {
        error() << "Can't parse" << sourceInformation.sourceFile();
    }

    if (type == Dump) {
        mData->logOutput += "\n";
        {
            Log stream(&mData->logOutput);
            stream << "symbols:\n";
            for (SymbolMap::const_iterator it = mData->symbols.begin(); it != mData->symbols.end(); ++it) {
                stream << it->first << it->second.toString(0) << '\n';
            }

            stream << "symbolnames:\n";
            for (SymbolNameMap::const_iterator it = mData->symbolNames.begin(); it != mData->symbolNames.end(); ++it) {
                stream << it->first << it->second << '\n';
            }
        }

        mData->symbols.clear();
        mData->symbolNames.clear();
        mData->references.clear();
    } else {
        mData->message = String::format<128>("%s in %dms. (%d syms, %d symNames, %d refs)",
                                             sourceInformation.sourceFile().toTilde().constData(),
                                             timer.elapsed(), mData->symbols.size(),
                                             mData->symbolNames.size(), mData->references.size());
    }
    if (std::shared_ptr<Project> proj = project.lock()) {
        proj->onJobFinished(mData);
    }
}

