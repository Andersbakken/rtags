#ifndef ProjectMessage_h
#define ProjectMessage_h

#include <List.h>
#include <ByteArray.h>
#include "Message.h"
#include "RTags.h"

class ProjectMessage : public Message
{
public:
    enum { MessageId = 6 };

    enum Flag {
        NoFlag = 0x0,
        UseDashB = 0x1,
        NoMakeTricks = 0x2
    };

    enum Type {
        NoType,
        MakefileType,
        GRTagsType,
        SmartType
    };

    ProjectMessage(Type type = NoType, const Path &path = Path());

    virtual int messageId() const { return MessageId; }

    Type type() const { return mType; }
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
    Type mType;
    Path mPath;
    List<ByteArray> mArgs, mExtraCompilerFlags;
    unsigned mFlags;
};

#endif
