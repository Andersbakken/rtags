#include "OutputMessage.h"
#include <QDataStream>

OutputMessage::OutputMessage(QObject *parent)
    : mLevel(0)
{
}

OutputMessage::OutputMessage(const QByteArray &name, int level, QObject *parent)
    : mName(name), mLevel(level)
{
}

QByteArray OutputMessage::name() const
{
    return mName;
}

int OutputMessage::level() const
{
    return mLevel;
}

QByteArray OutputMessage::toByteArray() const
{
    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << mName << mLevel;
    }
    return data;
}

void OutputMessage::fromByteArray(const QByteArray& data)
{
    QDataStream stream(data);
    stream >> mName >> mLevel;
}
