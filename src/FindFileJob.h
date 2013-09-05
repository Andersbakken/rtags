#ifndef FindFileJob_h
#define FindFileJob_h

#include <rct/String.h>
#include <rct/List.h>
#include "RTagsClang.h"
#include "Job.h"
#include "Location.h"
#include <rct/RegExp.h>

class FindFileJob : public Job
{
public:
    FindFileJob(const QueryMessage &query, const std::shared_ptr<Project> &project);
protected:
    virtual void execute();
private:
    String mPattern;
    RegExp mRegExp;
};

#endif
