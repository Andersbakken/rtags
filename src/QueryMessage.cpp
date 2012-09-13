#include "QueryMessage.h"
#include "RTags.h"
#include <Serializer.h>

QueryMessage::QueryMessage(Type type, const ByteArray& query, unsigned flags,
                           int max, const Map<Path, ByteArray> &unsavedFiles)
    : mQuery(query), mType(type), mFlags(flags), mMax(max), mUnsavedFiles(unsavedFiles)
{
}

ByteArray QueryMessage::encode() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << mQuery << mType << mFlags << mMax << mPathFilters << mUnsavedFiles;
    }
    return data;
}

void QueryMessage::fromData(const char *data, int size)
{
    Deserializer stream(data, size);
    stream >> mQuery >> mType >> mFlags >> mMax >> mPathFilters >> mUnsavedFiles;
}

unsigned QueryMessage::keyFlags(unsigned queryFlags)
{
    unsigned ret = Location::NoFlag;
    if (!(queryFlags & QueryMessage::NoContext))
        ret |= Location::ShowContext;
    if (queryFlags & QueryMessage::LineNumbers)
        ret |= Location::ShowLineNumbers;
    return ret;
}
