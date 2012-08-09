#ifndef GRTags_h
#define GRTags_h

#include "EventReceiver.h"
#include "Path.h"
#include "List.h"

class GRTags : public EventReceiver
{
public:
    GRTags(const Path &root);
    void recurseDirs();
    void onRecurseJobFinished(const List<Path> &mPaths);
private:
    const Path mSrcRoot;
};

#endif
