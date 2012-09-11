#ifndef SHA256_H
#define SHA256_H

#include <ByteArray.h>

class SHA256Private;

class SHA256
{
public:
    SHA256();
    ~SHA256();

    enum MapType { Raw, Hex };

    void update(const ByteArray& data);
    void update(const char* data, unsigned int size);

    void reset();

    ByteArray hash(MapType type = Hex) const;

    static ByteArray hash(const ByteArray& data, MapType type = Hex);
    static ByteArray hash(const char* data, unsigned int size, MapType type = Hex);

private:
    SHA256Private* priv;
};

#endif // SHA256_H
