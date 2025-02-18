/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#include "Token.h"

#include "rct/List.h"

String Token::toString() const
{
    String ret;
    {
        Log log(&ret);
        log << "Location:" << location
            << "Offset:" << offset
            << "Length:" << length
            << "Kind:" << kind
            << "\nSpelling:";
        if (spelling.contains('\n')) {
            for (const String &line : spelling.split('\n')) {
                log << "\n " << line;
            }
        } else {
            log << "\n " << spelling;
        }
    }
    return ret;

}
