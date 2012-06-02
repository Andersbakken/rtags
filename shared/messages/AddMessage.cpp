#include "AddMessage.h"
#include <QDataStream>

AddMessage::AddMessage(QObject *parent)
    : Message(parent)
{
}

AddMessage::AddMessage(RTags::UnitType type, const QByteArray &input, const QByteArray &output, const Path &compiler,
                       const QList<QByteArray> &args, const QList<QByteArray> &pchs, QObject *parent)
    : Message(parent), mType(type), mInput(input), mOutput(output), mArgs(args), mCompiler(compiler)
{
    foreach (const QByteArray &arg, pchs) {
        Q_ASSERT(!arg.isEmpty());
        switch (type) {
        case RTags::CompileCPlusPlus:
        case RTags::PchCPlusPlus:
            mArgs.append("-include-pch");
            mArgs.append(arg);
            break;
        default:
            break;
        }
    }
}

QByteArray AddMessage::toByteArray() const
{
    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << static_cast<int>(mType) << mInput << mOutput << mCompiler << mArgs;
    }
    return data;
}

void AddMessage::fromByteArray(const QByteArray &data)
{
    int t;
    QDataStream stream(data);
    stream >> t >> mInput >> mOutput >> mCompiler >> mArgs;
    mType = static_cast<RTags::UnitType>(t);
}
