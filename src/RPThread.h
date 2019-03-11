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

#ifndef RPTHREAD_H
#define RPTHREAD_H

#include <rct/Thread.h>
#include <rct/String.h>
#include <rct/SignalSlot.h>
#include <rct/Path.h>

class RPThread : public Thread
{
public:
    RPThread();
    void kill();
    virtual void run() override;
    Signal<std::function<void(RPThread*)> > &readyReadStdOut() { return mReadyReadStdOut; }
    Signal<std::function<void(RPThread*)> > &finished() { return mFinished; }
    String readAllStdOut();
    String readAllStdErr();
    bool start(const Path &, const List<String> &args);
    String errorString() const;
    pid_t pid() const;
    int returnCode() const;
    void write(const String &string);
private:
    Signal<std::function<void(RPThread*)> > mReadyReadStdOut, mFinished;
};

#endif
