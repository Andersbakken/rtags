#ifndef ResponseMessage_h
#define ResponseMessage_h

#include "Message.h"
#include "RTags.h"

class ResponseMessage : public Message
{
    Q_OBJECT
public:
    enum { MessageId = 3 };

    Q_INVOKABLE ResponseMessage(QObject *parent = 0)
        : Message(parent)
    {}
    ResponseMessage(const ByteArray &data, QObject *parent = 0)
        : Message(parent), mData(data)
    {}
    ResponseMessage(const List<ByteArray> &data, QObject *parent = 0)
        : Message(parent), mData(RTags::join(data, "\n"))
    {}

    virtual int messageId() const { return MessageId; }
    ByteArray data() const { return mData; }
    void setData(const ByteArray &data) { mData = data; }
    ByteArray toByteArray() const { return mData; }
    Q_INVOKABLE void fromByteArray(const ByteArray &data) { mData = data; }
private:
    ByteArray mData;
};

#endif
