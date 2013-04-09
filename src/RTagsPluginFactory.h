#ifndef RTagsPluginFactory_h
#define RTagsPluginFactory_h

#include <rct/Memory.h>
#include <rct/Path.h>
#include <rct/List.h>
#include <rct/Plugin.h>
#include "RTagsPlugin.h"

class RTagsPluginFactory
{
public:
    ~RTagsPluginFactory()
    {
        mPlugins.clear();
    }
    bool addPlugin(const Path &plugin)
    {
        mPlugins.append(plugin);
        if (!mPlugins.last().instance()) {
            mPlugins.removeLast();
            return false;
        }
        return true;
    }

    shared_ptr<IndexerJob> createJob(const shared_ptr<Project> &project, IndexerJob::Type type, const SourceInformation &sourceInformation);
    shared_ptr<IndexerJob> createJob(const QueryMessage &msg, const shared_ptr<Project> &project, const SourceInformation &sourceInformation);
private:
    List<Plugin<RTagsPlugin> > mPlugins;
};

#endif
