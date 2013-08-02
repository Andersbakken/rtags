#ifndef JSONPARSER_H
#define JSONPARSER_H

#include <rct/Value.h>
#include <rct/String.h>

class JSONParser
{
public:
    JSONParser() { }
    JSONParser(const String& json) { parse(json); }

    bool parse(const String& json);
    bool isValid() { return mRoot.type() != Value::Type_Invalid; }

    const Value& root() const { return mRoot; }

private:
    Value mRoot;
};

#endif
