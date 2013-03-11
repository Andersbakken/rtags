#ifndef CompileMessage_h
#define CompileMessage_h

#include <rct/List.h>
#include <rct/String.h>
#include "ClientMessage.h"
#include "RTags.h"

class CompileMessage : public ClientMessage
{
public:
    enum { MessageId = ProjectId };

    CompileMessage(const Path &path = Path(), const String &args = String());

    Path path() const { return mPath; }

    String arguments() const { return mArgs; }
    void setArguments(const String &arguments) { mArgs = arguments; }

    virtual void encode(Serializer &serializer) const;
    virtual void decode(Deserializer &deserializer);
private:
    Path mPath;
    String mArgs;
};

#endif
