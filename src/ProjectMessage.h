#ifndef ProjectMessage_h
#define ProjectMessage_h

#include "List.h"
#include "ByteArray.h"
#include "ClientMessage.h"
#include "RTags.h"

class ProjectMessage : public ClientMessage
{
public:
    enum { MessageId = 6 };

    enum Flag {
        NoFlag = 0x0,
        UseDashB = 0x1,
        Automake = 0x2
    };

    ProjectMessage(RTags::ProjectType type = RTags::Type_None, const Path &path = Path());

    virtual int messageId() const { return MessageId; }

    RTags::ProjectType type() const { return mType; }
    Path path() const { return mPath; }

    List<ByteArray> arguments() const { return mArgs; }
    void setArguments(const List<ByteArray> &arguments) { mArgs = arguments; }

    List<ByteArray> extraCompilerFlags() const { return mExtraCompilerFlags; }
    void setExtraFlags(const List<ByteArray> &f) { mExtraCompilerFlags = f; }

    unsigned flags() const { return mFlags; }
    void setFlags(unsigned flags) { mFlags = flags; }

    ByteArray encode() const;
    void fromData(const char *data, int size);
private:
    RTags::ProjectType mType;
    Path mPath;
    List<ByteArray> mArgs, mExtraCompilerFlags;
    unsigned mFlags;
};

#endif
