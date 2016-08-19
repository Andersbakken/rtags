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

#ifndef ProcThread_h
#define ProcThread_h

#include <rct/Hash.h>
#include <rct/Thread.h>
#include <rct/SignalSlot.h>
#include <rct/String.h>
#include <mutex>
#include <condition_variable>
#include <time.h>

class ProcThread : public Thread
{
public:
    ProcThread(int interval);
    ~ProcThread();

    void stop();
    virtual void run();
    Signal<std::function<void(String)> > &command() { return mCommand; }
private:
    void readProc();

    Signal<std::function<void(String)> > mCommand;
    std::mutex mMutex;
    std::condition_variable mCond;
    int mInterval;

    struct Node {
        Node()
            : creationData(0), marked(false)
        {}
        time_t creationData;
        bool marked;
    };
    Hash<unsigned long long, Node> mSeen;
};

#endif
