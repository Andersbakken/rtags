#include "OutputMessage.h"
#include <QDataStream>

OutputMessage::OutputMessage(QObject *parent)
    : mLevel(0)
{
}

OutputMessage::OutputMessage(const ByteArray &name, int level, QObject *parent)
    : mName(name), mLevel(level)
{
}

ByteArray OutputMessage::name() const
{
    return mName;
}

int OutputMessage::level() const
{
    return mLevel;
}

ByteArray OutputMessage::toByteArray() const
{
    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << mName << mLevel;
    }
    return ByteArray(data.constData(), data.size());
}

void OutputMessage::fromByteArray(const ByteArray &data)
{
    const QByteArray ba = QByteArray::fromRawData(data.constData(), data.size());
    QDataStream stream(ba);
    stream >> mName >> mLevel;
}
