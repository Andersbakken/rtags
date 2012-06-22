#ifndef EVENT_H
#define EVENT_H

class Event
{
public:
    Event(int type) {}
    virtual ~Event() {}

    int type() const { return mType; }

private:
    int mType;
};

#endif
