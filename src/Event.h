#ifndef EVENT_H
#define EVENT_H

class Event
{
public:
    Event(int type) : mType(type) {}
    virtual ~Event() {}

    int type() const { return mType; }

private:
    const int mType;
};

#endif
