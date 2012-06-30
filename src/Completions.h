#ifndef Completions_h
#define Completions_h

#include "EventReceiver.h"

class Completions : public EventReceiver
{
public:
    Completions();
    ~Completions();
    virtual void event(const Event *event);

    ByteArray completions(const ByteArray &query, unsigned queryFlags, const Map<Path, ByteArray> &unsavedFiles);
};

#endif
