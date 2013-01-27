#ifndef JSONJob_h
#define JSONJob_h

#include "ByteArray.h"
#include "Job.h"
#include "RTags.h"

class QueryMessage;
class JSONJob : public Job
{
public:
    JSONJob(const QueryMessage &query, const shared_ptr<Project> &project);
protected:
    virtual void execute();
    void processFile(uint32_t fileId, const Path &path, const SymbolMap &map);
private:
    const Match match;
    int mSrcRootLength;
};

#endif
