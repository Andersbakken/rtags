#include "MakefileMessage.h"
#include <QDataStream>

MakefileMessage::MakefileMessage(QObject *parent)
    : Message(parent)
{
}

MakefileMessage::MakefileMessage(const Path &makefile, const QList<ByteArray> &args,
                                 const QList<ByteArray> &extraFlags, QObject *parent)
    : Message(parent), mMakefile(makefile), mArgs(args), mExtraFlags(extraFlags)
{
}

ByteArray MakefileMessage::toByteArray() const
{
    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << mMakefile << mArgs << mExtraFlags;
    }
    return ByteArray(data.constData(), data.size());
}

void MakefileMessage::fromByteArray(const ByteArray &data)
{
    QByteArray ba = QByteArray::fromRawData(data.constData(), data.size());
    QDataStream stream(ba);
    stream >> mMakefile >> mArgs >> mExtraFlags;
}
