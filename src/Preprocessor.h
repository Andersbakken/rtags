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

#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include <memory>

#include "Source.h"

class Connection;
class Process;
class Preprocessor
{
public:
    Preprocessor(const Source &source, const std::shared_ptr<Connection> &connection);

    void preprocess();
private:
    void onProcessFinished();
    const Source mSource;
    List<String> mArgs;
    std::shared_ptr<Connection> mConnection;
    std::unique_ptr<Process> mProcess;
};

#endif
