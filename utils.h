#ifndef utils_h
#define utils_h

#include <QtNetwork>
#include <QtCore>

enum { SizeOfSize = sizeof(qint16) };
static inline int port()
{
    enum { DefaultPort = 6767 };
    QSettings settings(QSettings::IniFormat, QSettings::UserScope,
                       QCoreApplication::organizationName(), QCoreApplication::applicationName());
    return settings.value("daemonPort", DefaultPort).toInt();
}
template <typename T>
static inline bool writeToSocket(QAbstractSocket *dev, const T &t)
{
    // format sizeof(qint16) header with number of bytes in the rest of the package,
    // after that a QDataStream'ed T
    QByteArray byteArray;
    {
        QDataStream ds(&byteArray, QIODevice::WriteOnly);
        ds << t;
    }
    const qint16 size = byteArray.size();
    return (dev->write(reinterpret_cast<const char*>(&size), SizeOfSize) == SizeOfSize
            && dev->write(byteArray) == size);
}

enum ReadState {
    Error,
    Finished,
    WaitForData
};
template <typename T>
static inline ReadState readFromSocket(QAbstractSocket *dev, T &t, qint16 &size)
{
    switch (dev->state()) {
    case QAbstractSocket::ConnectingState:
    case QAbstractSocket::HostLookupState:
        return WaitForData;
    case QAbstractSocket::ConnectedState:
        break;
    default:
        return Error;
    }
    int available = dev->bytesAvailable();
    if (size == -1) {
        if (available < SizeOfSize)
            return WaitForData;
        if (dev->read(reinterpret_cast<char*>(&size), SizeOfSize) != SizeOfSize) {
            return Error;
        }
        available -= SizeOfSize;
    }
    if (available < size)
        return WaitForData;
    const QByteArray ba = dev->read(size);
    if (ba.size() != size)
        return Error;
    QDataStream ds(ba);
    ds >> t;
    return Finished;
}

#endif
