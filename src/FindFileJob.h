#ifndef FindFileJob_h
#define FindFileJob_h

#include <ByteArray.h>
#include <List.h>
#include "RTags.h"
#include "Job.h"
#include "Location.h"
#include "RegExp.h"

class FindFileJob : public Job
{
public:
    FindFileJob(const QueryMessage &query);
protected:
    virtual void execute();
private:
    ByteArray mPattern;
    RegExp mRegExp;
};

#endif
