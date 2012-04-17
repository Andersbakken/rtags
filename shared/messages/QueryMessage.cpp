#include "QueryMessage.h"
#include "RTags.h"
#include <QDataStream>

QueryMessage::QueryMessage(QObject* parent)
    : Message(parent), mType(FollowLocation), mFlags(0)
{
}

QueryMessage::QueryMessage(const QByteArray& query, Type type, int flags,
                           const QHash<Path, QByteArray> &unsavedFiles, QObject* parent)
    : Message(parent), mType(type), mFlags(flags), mUnsavedFiles(unsavedFiles)
{
    mQuery.append(query);
}

QueryMessage::QueryMessage(const QList<QByteArray>& query, Type type, int flags, QObject* parent)
    : Message(parent), mQuery(query), mType(type), mFlags(flags)
{
}

QueryMessage::QueryMessage(const QList<QByteArray>& query, Type type, QObject* parent)
    : Message(parent), mQuery(query), mType(type), mFlags(0)
{
}

QByteArray QueryMessage::toByteArray() const
{
    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << mQuery << static_cast<int>(mType) << mFlags << mUnsavedFiles;
    }
    return data;
}

void QueryMessage::fromByteArray(const QByteArray& data)
{
    int t;
    QDataStream stream(data);
    stream >> mQuery >> t >> mFlags >> mUnsavedFiles;
    mType = static_cast<Type>(t);
}

unsigned QueryMessage::keyFlags() const
{
    unsigned ret = RTags::Location::NoFlag;
    if (!(mFlags & QueryMessage::NoContext))
        ret |= RTags::Location::ShowContext;
    if (mFlags & QueryMessage::LineNumbers)
        ret |= RTags::Location::ShowLineNumbers;
    return ret;
}
