#include "QueryMessage.h"
#include "RTags.h"
#include <QDataStream>

QueryMessage::QueryMessage(QObject* parent)
    : Message(parent), mType(Response), mFlags(0)
{
}

QueryMessage::QueryMessage(Type type, const QByteArray& query, unsigned flags,
                           const QHash<Path, QByteArray> &unsavedFiles, const QList<QByteArray> &pathFilters,
                           QObject* parent)
    : Message(parent), mType(type), mFlags(flags), mUnsavedFiles(unsavedFiles), mPathFilters(pathFilters)
{
    qSort(mPathFilters);
    mQuery.append(query);
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
        stream << mQuery << static_cast<int>(mType) << mFlags << mUnsavedFiles << mPathFilters;
    }
    return data;
}

void QueryMessage::fromByteArray(const QByteArray& data)
{
    int t;
    QDataStream stream(data);
    stream >> mQuery >> t >> mFlags >> mUnsavedFiles >> mPathFilters;
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

