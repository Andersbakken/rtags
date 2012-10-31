#ifndef ResponseMessage_h
#define ResponseMessage_h

#include "Message.h"
#include "RTags.h"

class ResponseMessage : public Message
{
public:
    enum { MessageId = ResponseId };

    ResponseMessage(const ByteArray &data = ByteArray())
        : mData(data)
    {}
    ResponseMessage(const List<ByteArray> &data)
        : mData(ByteArray::join(data, "\n"))
    {}

    virtual int messageId() const { return MessageId; }
    ByteArray data() const { return mData; }
    void setData(const ByteArray &data) { mData = data; }
    ByteArray encode() const { return mData; }
    void fromData(const char *data, int size) { mData = ByteArray(data, size); }
private:
    ByteArray mData;
};

#endif
