#ifndef MMAPDEVICE_H
#define MMAPDEVICE_H

#include <QIODevice>

class Mmap;

class MmapDevice : public QIODevice
{
    Q_OBJECT
public:
    MmapDevice(Mmap* mmap, QObject *parent = 0);

    bool isSequential() const;
    bool atEnd() const;

protected:
    qint64 readData(char *data, qint64 maxlen);
    qint64 writeData(const char *data, qint64 len);

private:
    Mmap* mMmap; // ...
};

#endif // MMAPDEVICE_H
