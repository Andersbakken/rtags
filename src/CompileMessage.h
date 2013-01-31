#ifndef CompileMessage_h
#define CompileMessage_h

#include "List.h"
#include "String.h"
#include "ClientMessage.h"
#include "RTags.h"

class CompileMessage : public ClientMessage
{
public:
    enum { MessageId = ProjectId };

    CompileMessage(const Path &path = Path(), const String &args = String());

    virtual int messageId() const { return MessageId; }

    Path path() const { return mPath; }

    String arguments() const { return mArgs; }
    void setArguments(const String &arguments) { mArgs = arguments; }

    String encode() const;
    void fromData(const char *data, int size);
private:
    Path mPath;
    String mArgs;
};

#endif
