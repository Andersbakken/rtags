#ifndef ListSymbolsJob_h
#define ListSymbolsJob_h

#include <rct/String.h>
#include <rct/List.h>
#include "QueryMessage.h"
#include "Job.h"

class ListSymbolsJob : public Job
{
public:
    ListSymbolsJob(const QueryMessage &query, const shared_ptr<Project> &proj);
protected:
    virtual void execute();
    List<String> imenu(const shared_ptr<Project> &project);
    List<String> listSymbols(const shared_ptr<Project> &project);
private:
    const String string;
};

#endif
