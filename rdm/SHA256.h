#ifndef SHA256_H
#define SHA256_H

#include <QByteArray>

class SHA256Private;

class SHA256
{
public:
    SHA256();
    ~SHA256();

    enum HashType { Raw, Hex };

    void update(const QByteArray& data);
    void update(const char* data, uint size);

    void reset();

    QByteArray hash(HashType type = Hex) const;

    static QByteArray hash(const QByteArray& data, HashType type = Hex);
    static QByteArray hash(const char* data, uint size, HashType type = Hex);

private:
    SHA256Private* priv;
};

#endif // SHA256_H
