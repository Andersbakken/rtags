#ifndef ProjectMessage_h
#define ProjectMessage_h

#include "List.h"
#include "ByteArray.h"
#include "ClientMessage.h"
#include "RTags.h"

class ProjectMessage : public ClientMessage
{
public:
    enum { MessageId = ProjectId };

    ProjectMessage(const Path &path = Path(), const ByteArray &args = ByteArray());

    virtual int messageId() const { return MessageId; }

    Path path() const { return mPath; }

    ByteArray arguments() const { return mArgs; }
    void setArguments(const ByteArray &arguments) { mArgs = arguments; }

    ByteArray encode() const;
    void fromData(const char *data, int size);
private:
    Path mPath;
    ByteArray mArgs;
};

#endif
