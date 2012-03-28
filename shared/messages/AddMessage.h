#ifndef ADDMESSAGE_H
#define ADDMESSAGE_H

#include <QObject>
#include <QList>
#include <QByteArray>
#include "Message.h"
#include "RTags.h"

class AddMessage : public Message
{
    Q_OBJECT
public:
    enum { MessageId = 1 };

    Q_INVOKABLE AddMessage(QObject* parent = 0);
    AddMessage(RTags::UnitType type, const QByteArray& input, const QByteArray& output,
               const QList<QByteArray>& arguments, const QList<QByteArray>& pchs,
               QObject* parent = 0);

    int messageId() const { return MessageId; }

    RTags::UnitType type() const { return mType; }
    QByteArray inputFile() const { return mInput; }
    QByteArray outputFile() const { return mOutput; }
    QList<QByteArray> arguments() const { return mArgs; }
    QList<QByteArray> pchs() const { return mPchs; }

    QByteArray toByteArray() const;
    Q_INVOKABLE void fromByteArray(const QByteArray& data);

private:
    RTags::UnitType mType;
    QByteArray mInput, mOutput;
    QList<QByteArray> mArgs, mPchs;
};

#endif // ADDMESSAGE_H
