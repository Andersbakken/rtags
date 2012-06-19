#ifndef SHA256_H
#define SHA256_H

#include <ByteArray.h>

class SHA256Private;

class SHA256
{
public:
    SHA256();
    ~SHA256();

    enum HashType { Raw, Hex };

    void update(const ByteArray& data);
    void update(const char* data, uint size);

    void reset();

    ByteArray hash(HashType type = Hex) const;

    static ByteArray hash(const ByteArray& data, HashType type = Hex);
    static ByteArray hash(const char* data, uint size, HashType type = Hex);

private:
    SHA256Private* priv;
};

#endif // SHA256_H
