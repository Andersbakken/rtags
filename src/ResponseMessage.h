#ifndef ResponseMessage_h
#define ResponseMessage_h

#include "Message.h"
#include "RTags.h"

class ResponseMessage : public Message
{
public:
    enum { MessageId = ResponseId };

    ResponseMessage(const String &data = String())
        : mData(data)
    {
        if (mData.endsWith('\n'))
            mData.chop(1);
    }
    ResponseMessage(const List<String> &data)
        : mData(String::join(data, "\n"))
    {
        if (mData.endsWith('\n'))
            mData.chop(1);
    }

    virtual int messageId() const { return MessageId; }
    String data() const { return mData; }
    void setData(const String &data) { mData = data; }
    String encode() const { return mData; }
    void fromData(const char *data, int size) { mData = String(data, size); }
private:
    String mData;
};

#endif
