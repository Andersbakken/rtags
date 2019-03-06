/* This file is part of RTags (http://rtags.net).

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

#ifndef RPClangIndexer_h
#define RPClangIndexer_h

#include "ClangIndexer.h"

struct Unit;
class RPClangIndexer : public ClangIndexer
{
public:
    RPClangIndexer();
    ~RPClangIndexer();

    bool exec(const String &data);
protected:
    virtual Location createLocation(const Path &file, unsigned int line, unsigned int col, bool *blocked = 0) override;
    virtual bool send(const std::shared_ptr<IndexDataMessage> &msg) override;
private:
    void onMessage(const std::shared_ptr<Message> &msg, const std::shared_ptr<Connection> &conn);

    uint32_t mVisitFileResponseMessageFileId { 0 };
    bool mVisitFileResponseMessageVisit { false };
    Path mSocketFile;
    int mVisitFileTimeout { 0 }, mIndexDataMessageTimeout { 0 };
    std::shared_ptr<Connection> mConnection;
};

#endif
