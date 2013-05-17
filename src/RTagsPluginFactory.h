#ifndef RTagsPluginFactory_h
#define RTagsPluginFactory_h

#include <rct/Tr1.h>
#include <rct/Path.h>
#include <rct/List.h>
#include <rct/Plugin.h>
#include "RTagsPlugin.h"

class RTagsPluginFactory
{
public:
    ~RTagsPluginFactory()
    {
        for (int i=0; i<mPlugins.size(); ++i) {
            delete mPlugins.at(i);
        }
    }
    bool addPlugin(const Path &plugin)
    {
        Plugin<RTagsPlugin> *p = new Plugin<RTagsPlugin>(plugin);
        if (!p->instance()) {
            delete p;
            return false;
        }

        mPlugins.append(p);
        return true;
    }

    shared_ptr<IndexerJob> createJob(const shared_ptr<Project> &project, IndexerJob::Type type,
                                     const SourceInformation &sourceInformation)
    {
        shared_ptr<IndexerJob> ret;
        for (int i=0; i<mPlugins.size(); ++i) {
            assert(mPlugins.at(i)->instance());
            ret = mPlugins.at(i)->instance()->createJob(project, type, sourceInformation);
            if (ret)
                break;
        }
        return ret;
    }
    shared_ptr<IndexerJob> createJob(const QueryMessage &msg, const shared_ptr<Project> &project,
                                     const SourceInformation &sourceInformation)
    {
        shared_ptr<IndexerJob> ret;
        for (int i=0; i<mPlugins.size(); ++i) {
            assert(mPlugins.at(i)->instance());
            ret = mPlugins.at(i)->instance()->createJob(msg, project, sourceInformation);
            if (ret)
                break;
        }
        return ret;

    }
private:
    List<Plugin<RTagsPlugin> *> mPlugins;
};

#endif
