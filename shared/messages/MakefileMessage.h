#ifndef MAKEFILEMESSAGE_H
#define MAKEFILEMESSAGE_H

#include <QObject>
#include <QList>
#include <QByteArray>
#include "Message.h"
#include "RTags.h"

class MakefileMessage : public Message
{
    Q_OBJECT
public:
    enum { MessageId = 6 };

    Q_INVOKABLE MakefileMessage(QObject *parent = 0);
    MakefileMessage(const Path &makefile, const QList<QByteArray> &arguments,
                    const QList<QByteArray> &extraFlags, QObject *parent = 0);

    int messageId() const { return MessageId; }

    Path makefile() const { return mMakefile; }
    QList<QByteArray> arguments() const { return mArgs; }
    QList<QByteArray> extraFlags() const { return mExtraFlags; }

    QByteArray toByteArray() const;
    Q_INVOKABLE void fromByteArray(const QByteArray &data);

private:
    Path mMakefile;
    QList<QByteArray> mArgs, mExtraFlags;
};

#endif // MAKEFILEMESSAGE_H
