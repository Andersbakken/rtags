#ifndef IniFile_h
#define IniFile_h

#include "Path.h"
#include "Map.h"
#include "List.h"
#include "ByteArray.h"

// consider only syncing when it goes out of scope, or is told to
class IniFile
{
public:
    IniFile(const Path &path);

    Path path() const { return mPath; }
    bool isValid() const { return mValid; }

    ByteArray error() const { return mError; }

    typedef Map<ByteArray, Map<ByteArray, ByteArray> > MapMap;
    const MapMap &values() const { return mValues; }
    bool setValue(const ByteArray &group, const ByteArray &key, const ByteArray &value = ByteArray());
    bool remove(const ByteArray &group, const ByteArray &key);
    bool removeGroup(const ByteArray &group);
    List<ByteArray> keys(const ByteArray &group) const { return mValues.value(group).keys(); }
    ByteArray value(const ByteArray &group, const ByteArray &key, const ByteArray &defaultValue = ByteArray()) const
    { return mValues.value(group).value(key, defaultValue); }
private:
    bool syncFromFile();
    bool syncToFile();
    const Path mPath;
    bool mValid;
    MapMap mValues;
    ByteArray mError;
};

#endif
