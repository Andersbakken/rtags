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

#include "Location.h"
#include "Server.h"
#include <rct/Rct.h>
#include "RTags.h"
Hash<Path, uint32_t> Location::sPathsToIds;
Hash<uint32_t, Path> Location::sIdsToPaths;
uint32_t Location::sLastId = 0;
std::mutex Location::sMutex;

String Location::key(unsigned flags) const
{
    if (isNull())
        return String();
    int extra = RTags::digits(mLine) + RTags::digits(mColumn) + 3;
    String ctx;
    if (flags & Location::ShowContext) {
        ctx += '\t';
        ctx += context();
        extra += ctx.size();
    }

    const Path p = path();

    String ret(p.size() + extra, '0');

    snprintf(ret.data(), ret.size() + extra + 1, "%s:%d:%d:%s", p.constData(),
             mLine, mColumn, ctx.constData());
    return ret;
}

String Location::context() const
{
    Path p = path();
    FILE *f = fopen(p.constData(), "r");
    if (f) {
        for (unsigned i=1; i<mLine; ++i) {
             Rct::readLine(f);
         }

         char buf[1024] = { '\0' };
         const int len = Rct::readLine(f, buf, 1023);
         fclose(f);
         return String(buf, len);
     }
     return String();
 }
