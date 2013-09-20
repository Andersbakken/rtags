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
    virtual std::shared_ptr<IndexerJob> createJob(const std::shared_ptr<Project> &project,
                                             IndexerJob::Type type,
                                             const SourceInformation &sourceInformation)
    {
        if (sourceInformation.isJS())
            return std::shared_ptr<IndexerJob>(new IndexerJobEsprima(project, type, sourceInformation));
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

IndexerJobEsprima::IndexerJobEsprima(const std::shared_ptr<Project> &project,
                                     Type type,
                                     const SourceInformation &sourceInformation)
    : IndexerJob(project, type, sourceInformation)
{}

IndexerJobEsprima::IndexerJobEsprima(const QueryMessage &msg,
                                     const std::shared_ptr<Project> &project,
                                     const SourceInformation &sourceInformation)
    : IndexerJob(msg, project, sourceInformation)
{
}

void IndexerJobEsprima::index()
{
    JSParser parser;
    if (!parser.init()) {
        error() << "Can't init JSParser for" << mSourceInformation.sourceFile();
        return;
    }
    if (isAborted())
        return;
    String dump;
    if (!parser.parse(mSourceInformation.sourceFile(), &mData->symbols, &mData->symbolNames,
                      mType == Dump ? 0 : &mData->dependencies, mType == Dump ? &dump : 0)) {
        error() << "Can't parse" << mSourceInformation.sourceFile();
    }
    mParseTime = time(0);

    if (mType == Dump) {
        dump += "\n";
        {
            Log stream(&dump);
            stream << "symbols:\n";
            for (Map<Location, CursorInfo>::const_iterator it = mData->symbols.begin(); it != mData->symbols.end(); ++it) {
                stream << it->first << it->second.toString(0) << '\n';
            }

            stream << "symbolnames:\n";
            for (Map<String, Set<Location> >::const_iterator it = mData->symbolNames.begin(); it != mData->symbolNames.end(); ++it) {
                stream << it->first << it->second << '\n';
            }

            assert(id() != -1);
        }
        write(dump);

        mData->symbols.clear();
        mData->symbolNames.clear();
    } else {
        mData->message = String::format<128>("%s in %dms. (%d syms, %d symNames, %d refs)",
                                             mSourceInformation.sourceFile().toTilde().constData(),
                                             static_cast<int>(mTimer.elapsed()) / 1000, mData->symbols.size(), mData->symbolNames.size(), mData->references.size());
    }
}

