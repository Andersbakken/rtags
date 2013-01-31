#ifndef SHA256_H
#define SHA256_H

#include "String.h"

class SHA256Private;

class SHA256
{
public:
    SHA256();
    ~SHA256();

    enum MapType { Raw, Hex };

    void update(const String& data);
    void update(const char* data, unsigned int size);

    void reset();

    String hash(MapType type = Hex) const;

    static String hash(const String& data, MapType type = Hex);
    static String hash(const char* data, unsigned int size, MapType type = Hex);

private:
    SHA256Private* priv;
};

#endif // SHA256_H
