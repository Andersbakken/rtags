#ifndef MAKEFILEMESSAGE_H
#define MAKEFILEMESSAGE_H

#include <List.h>
#include <ByteArray.h>
#include "Message.h"
#include "RTags.h"

class MakefileMessage : public Message
{
public:
    enum { MessageId = 6 };

    enum Flag {
        NoFlag = 0x0,
        UseDashB = 0x1,
        NoMakeTricks = 0x2
    };

    MakefileMessage(const Path &makefile = Path());

    virtual int messageId() const { return MessageId; }

    Path makefile() const { return mMakefile; }

    List<ByteArray> arguments() const { return mArgs; }
    void setArguments(const List<ByteArray> &arguments) { mArgs = arguments; }

    List<ByteArray> extraCompilerFlags() const { return mExtraCompilerFlags; }
    void setExtraFlags(const List<ByteArray> &f) { mExtraCompilerFlags = f; }

    unsigned flags() const { return mFlags; }
    void setFlags(unsigned flags) { mFlags = flags; }

    ByteArray encode() const;
    void fromData(const char *data, int size);
private:
    Path mMakefile;
    List<ByteArray> mArgs, mExtraCompilerFlags;
    unsigned mFlags;
};

#endif // MAKEFILEMESSAGE_H
