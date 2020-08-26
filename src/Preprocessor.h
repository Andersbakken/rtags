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

#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include <memory>

#include "Source.h"
#include "rct/List.h"
#include "rct/String.h"

class Connection;
class Process;

class Preprocessor
{
public:
    enum Mode { Preprocess, Asm };

    Preprocessor(Mode mode, const Source &source, const std::shared_ptr<Connection> &connection);
    ~Preprocessor();

    void preprocess();
private:
    void onProcessFinished();
    Source mSource;
    List<String> mArgs;
    std::shared_ptr<Connection> mConnection;
    std::unique_ptr<Process> mProcess;
};

#endif
