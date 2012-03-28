#include "AddMessage.h"
#include <QDataStream>

AddMessage::AddMessage(QObject* parent)
    : Message(parent)
{
}

AddMessage::AddMessage(RTags::UnitType type, const QByteArray& input, const QByteArray& output,
                       const QList<QByteArray>& args, const QList<QByteArray>& pchs,
                       QObject* parent)
    : Message(parent), mType(type), mInput(input), mOutput(output), mArgs(args), mPchs(pchs)
{
}

QByteArray AddMessage::toByteArray() const
{
    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << static_cast<int>(mType) << mInput << mOutput << mArgs << mPchs;
    }
    return data;
}

void AddMessage::fromByteArray(const QByteArray& data)
{
    int t;
    QDataStream stream(data);
    stream >> t >> mInput >> mOutput >> mArgs >> mPchs;
    mType = static_cast<RTags::UnitType>(t);
}
