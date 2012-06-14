#include "MakefileMessage.h"
#include <QDataStream>

MakefileMessage::MakefileMessage(QObject *parent)
    : Message(parent)
{
}

MakefileMessage::MakefileMessage(const Path &makefile, const QList<QByteArray> &args,
                                 const QList<QByteArray> &extraFlags, QObject *parent)
    : Message(parent), mMakefile(makefile), mArgs(args), mExtraFlags(extraFlags)
{
}

QByteArray MakefileMessage::toByteArray() const
{
    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << mMakefile << mArgs << mExtraFlags;
    }
    return data;
}

void MakefileMessage::fromByteArray(const QByteArray &data)
{
    QDataStream stream(data);
    stream >> mMakefile >> mArgs >> mExtraFlags;
}
