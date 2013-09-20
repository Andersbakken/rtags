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

#ifndef ReparseJob_h
#define ReparseJob_h

#include <rct/ThreadPool.h>
#include <rct/Path.h>
#include <clang-c/Index.h>
#include <Project.h>

class ReparseJob : public ThreadPool::Job
{
public:
    ReparseJob(CXTranslationUnit unit, const Path &path, const List<String> &args, const String &unsaved,
               const std::shared_ptr<Project> &project)
        : mUnit(unit), mPath(path), mArgs(args), mUnsaved(unsaved), mProject(project)
    {}

    virtual void run()
    {
        CXUnsavedFile unsaved = { mPath.constData(),
                                  mUnsaved.constData(),
                                  static_cast<unsigned long>(mUnsaved.size()) };

        RTags::reparseTranslationUnit(mUnit, &unsaved, mUnsaved.isEmpty() ? 1 : 0);
        if (mUnit) {
            std::shared_ptr<Project> project = mProject.lock();
            if (project) {
                project->addToCache(mPath, mArgs, mUnit, 2);
                // error() << "Did a reparse" << mPath;
                mUnit = 0;
            }
        }

        if (mUnit)
            clang_disposeTranslationUnit(mUnit);
    }
private:
    CXTranslationUnit mUnit;
    const Path mPath;
    const List<String> mArgs;
    const String mUnsaved;
    std::weak_ptr<Project> mProject;
};

#endif
