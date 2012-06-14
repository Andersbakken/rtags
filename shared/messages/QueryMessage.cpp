#include "QueryMessage.h"
#include "RTags.h"
#include <QDataStream>

QueryMessage::QueryMessage(QObject *parent)
    : Message(parent), mType(Invalid), mFlags(0)
{
}

QueryMessage::QueryMessage(Type type, const QByteArray& query, unsigned flags, QObject* parent)
    : Message(parent), mType(type), mFlags(flags)
{
    mQuery.append(query);
}

QByteArray QueryMessage::toByteArray() const
{
    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << mQuery << static_cast<int>(mType) << mFlags << mPathFilters;
    }
    return data;
}

void QueryMessage::fromByteArray(const QByteArray& data)
{
    int t;
    QDataStream stream(data);
    stream >> mQuery >> t;
    mType = static_cast<Type>(t);
    stream >> mFlags >> mPathFilters;
}

unsigned QueryMessage::keyFlags(unsigned queryFlags)
{
    unsigned ret = RTags::NoFlag;
    if (!(queryFlags & QueryMessage::NoContext))
        ret |= RTags::ShowContext;
    if (queryFlags & QueryMessage::LineNumbers)
        ret |= RTags::ShowLineNumbers;
    return ret;
}

