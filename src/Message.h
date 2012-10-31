#ifndef MESSAGE_H
#define MESSAGE_H

class Message
{
public:
    enum {
        CompletionId = 1,
        QueryId,
        ProjectId,
        ResponseId,
        CreateOutputId
    };

    Message() {}
    virtual ~Message() {}
    virtual int messageId() const = 0;
};

#endif // MESSAGE_H
