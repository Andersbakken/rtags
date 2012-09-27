#ifndef ClientMessage_h
#define ClientMessage_h

#include "Message.h"
#include "ByteArray.h"

class ClientMessage : public Message
{
public:
    void init(int argc, char **argv)
    {
        mRaw.reserve(256);
        for (int i=0; i<argc; ++i) {
            if (i > 0)
                mRaw.append(' ');
            mRaw.append(argv[i]);
        }
    }
    ByteArray raw() const
    {
        return mRaw;
    }
protected:
    ByteArray mRaw;
};

#endif
