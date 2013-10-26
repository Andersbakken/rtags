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

#ifndef RTagsPluginFactory_h
#define RTagsPluginFactory_h

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

    std::shared_ptr<IndexerJob> createJob(IndexerJob::IndexType type,
                                          const std::shared_ptr<Project> &project,
                                          const Source &source)
    {
        std::shared_ptr<IndexerJob> ret;
        for (int i=0; i<mPlugins.size(); ++i) {
            assert(mPlugins.at(i)->instance());
            ret = mPlugins.at(i)->instance()->createJob(type, project, source);
            if (ret)
                break;
        }
        return ret;
    }
private:
    List<Plugin<RTagsPlugin> *> mPlugins;
};

#endif
