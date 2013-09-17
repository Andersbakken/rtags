/* This file is part of RTags.

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

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
