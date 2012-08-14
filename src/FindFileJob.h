#ifndef FindFileJob_h
#define FindFileJob_h

#include <ByteArray.h>
#include <List.h>
#include "RTags.h"
#include "Job.h"
#include "Location.h"
#include "RegExp.h"

class GRTags;
class FindFileJob : public Job
{
public:
    FindFileJob(const std::tr1::shared_ptr<GRTags> &tags, const QueryMessage &query);
protected:
    virtual void execute();
private:
    std::tr1::shared_ptr<GRTags> mTags;
    ByteArray mPattern;
    RegExp mRegExp;
};

#endif
