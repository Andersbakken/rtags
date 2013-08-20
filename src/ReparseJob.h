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
               const shared_ptr<Project> &project)
        : mUnit(unit), mPath(path), mArgs(args), mUnsaved(unsaved), mProject(project)
    {}

    virtual void run()
    {
        CXUnsavedFile unsaved = { mPath.constData(),
                                  mUnsaved.constData(),
                                  static_cast<unsigned long>(mUnsaved.size()) };

        RTags::reparseTranslationUnit(mUnit, &unsaved, mUnsaved.isEmpty() ? 1 : 0);
        if (mUnit) {
            shared_ptr<Project> project = mProject.lock();
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
    weak_ptr<Project> mProject;
};

#endif
