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

#ifndef CompletionJob_h
#define CompletionJob_h

#include "Job.h"
#include <rct/List.h>
#include <rct/String.h>
#include <rct/Path.h>
#include <clang-c/Index.h>

class CompletionJob : public Job
{
public:
    enum Type {
        Stream,
        Sync
    };
    enum { SendThreshold = 500 };

    CompletionJob(const std::shared_ptr<Project> &project, Type type);

    void init(CXTranslationUnit unit, const Path &path, const List<String> &args,
              int line, int column, int pos, const String &unsaved, int parseCount);

    virtual void execute();
    Signal<std::function<void(Path, int)> > &finished() { return mFinished; }
    Type type() const { return mType; }
private:
    void processDiagnostics(CXCodeCompleteResults* results);

private:
    CXTranslationUnit mUnit;
    Path mPath;
    List<String> mArgs;
    int mLine, mColumn, mPos, mParseCount;
    String mUnsaved;
    Signal<std::function<void(Path, int)> > mFinished;
    const Type mType;
};

#endif
