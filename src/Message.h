#ifndef MESSAGE_H
#define MESSAGE_H

class Message
{
public:
    Message() {}
    virtual ~Message() {}
    virtual int messageId() const = 0;
};

#endif // MESSAGE_H
