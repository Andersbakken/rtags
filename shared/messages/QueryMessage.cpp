#include "QueryMessage.h"
#include "RTags.h"
#include "Location.h"
#include <QDataStream>

QueryMessage::QueryMessage(QObject* parent)
    : Message(parent), mType(Response), mFlags(0)
{
}

QueryMessage::QueryMessage(Type type, const QByteArray& query, unsigned flags, QObject* parent)
    : Message(parent), mType(type), mFlags(flags)
{
    mQuery.append(query);
}

QueryMessage::QueryMessage(const QByteArray &msg)
    : Message(0), mQuery(QList<QByteArray>() << msg), mType(Response), mFlags(0)
{
}

QueryMessage::QueryMessage(const QList<QByteArray> &msg)
    : Message(0), mQuery(msg), mType(Response), mFlags(0)
{
}

QByteArray QueryMessage::toByteArray() const
{
    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << mQuery << static_cast<int>(mType);
        if (mType != Response)
            stream << mFlags << mUnsavedFiles << mPathFilters;
    }
    return data;
}

void QueryMessage::fromByteArray(const QByteArray& data)
{
    int t;
    QDataStream stream(data);
    stream >> mQuery >> t;
    mType = static_cast<Type>(t);
    if (mType != Response)
        stream >> mFlags >> mUnsavedFiles >> mPathFilters;
}

unsigned QueryMessage::keyFlags() const
{
    unsigned ret = Location::NoFlag;
    if (!(mFlags & QueryMessage::NoContext))
        ret |= Location::ShowContext;
    if (mFlags & QueryMessage::LineNumbers)
        ret |= Location::ShowLineNumbers;
    return ret;
}

