#ifndef ResponseMessage_h
#define ResponseMessage_h

#include "Message.h"
#include "RTags.h"

class ResponseMessage : public Message
{
    Q_OBJECT
public:
    enum { MessageId = 3 };

    Q_INVOKABLE ResponseMessage(QObject *parent)
        : Message(parent)
    {}
    ResponseMessage(const QByteArray &data, QObject *parent = 0)
        : Message(parent), mData(data)
    {}
    ResponseMessage(const QList<QByteArray> &data, QObject *parent = 0)
        : Message(parent), mData(RTags::join(data, "\n"))
    {}

    virtual int messageId() const { return MessageId; }
    QByteArray data() const { return mData; }
    void setData(const QByteArray &data) { mData = data; }
    QByteArray toByteArray() const { return mData; }
    Q_INVOKABLE void fromByteArray(const QByteArray &data) { mData = data; }
private:
    QByteArray mData;
};

#endif
