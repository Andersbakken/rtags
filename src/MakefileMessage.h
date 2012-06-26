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

    MakefileMessage(const Path &makefile = Path(),
                    const List<ByteArray> &arguments = List<ByteArray>(),
                    const List<ByteArray> &extraFlags = List<ByteArray>());

    virtual int messageId() const { return MessageId; }

    Path makefile() const { return mMakefile; }
    List<ByteArray> arguments() const { return mArgs; }
    List<ByteArray> extraFlags() const { return mExtraFlags; }

    ByteArray encode() const;
    void fromData(const char *data, int size);

private:
    Path mMakefile;
    List<ByteArray> mArgs, mExtraFlags;
};

#endif // MAKEFILEMESSAGE_H
