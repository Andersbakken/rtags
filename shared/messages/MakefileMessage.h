#ifndef MAKEFILEMESSAGE_H
#define MAKEFILEMESSAGE_H

#include <QObject>
#include <List.h>
#include <ByteArray.h>
#include "Message.h"
#include "RTags.h"

class MakefileMessage : public Message
{
    Q_OBJECT
public:
    enum { MessageId = 6 };

    Q_INVOKABLE MakefileMessage(QObject *parent = 0);
    MakefileMessage(const Path &makefile, const List<ByteArray> &arguments,
                    const List<ByteArray> &extraFlags, QObject *parent = 0);

    int messageId() const { return MessageId; }

    Path makefile() const { return mMakefile; }
    List<ByteArray> arguments() const { return mArgs; }
    List<ByteArray> extraFlags() const { return mExtraFlags; }

    ByteArray toByteArray() const;
    Q_INVOKABLE void fromByteArray(const ByteArray &data);

private:
    Path mMakefile;
    List<ByteArray> mArgs, mExtraFlags;
};

#endif // MAKEFILEMESSAGE_H
