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

#ifndef MessageQueue_h
#define MessageQueue_h

#include <memory>
#include <mutex>
#include <condition_variable>
#include <deque>

class Message;
class Connection;

struct PendingMessage {
    std::shared_ptr<Message> message;
    std::shared_ptr<Connection> connection;

    PendingMessage(const std::shared_ptr<Message> &msg, const std::shared_ptr<Connection> &conn)
        : message(msg), connection(conn)
    {}
};

class MessageQueue
{
public:
    MessageQueue() : mShutdown(false) {}

    void enqueue(const std::shared_ptr<Message> &message, const std::shared_ptr<Connection> &connection)
    {
        std::unique_lock<std::mutex> lock(mMutex);
        mQueue.emplace_back(message, connection);
        mCondition.notify_one();
    }

    bool dequeue(PendingMessage &msg, int timeoutMs = -1)
    {
        std::unique_lock<std::mutex> lock(mMutex);

        if (timeoutMs < 0) {
            mCondition.wait(lock, [this] { return !mQueue.empty() || mShutdown; });
        } else {
            mCondition.wait_for(lock, std::chrono::milliseconds(timeoutMs),
                               [this] { return !mQueue.empty() || mShutdown; });
        }

        if (mShutdown && mQueue.empty())
            return false;

        if (mQueue.empty())
            return false;

        msg = std::move(mQueue.front());
        mQueue.pop_front();
        return true;
    }

    void shutdown()
    {
        std::unique_lock<std::mutex> lock(mMutex);
        mShutdown = true;
        mCondition.notify_all();
    }

    size_t size() const
    {
        std::unique_lock<std::mutex> lock(mMutex);
        return mQueue.size();
    }

private:
    mutable std::mutex mMutex;
    std::condition_variable mCondition;
    std::deque<PendingMessage> mQueue;
    bool mShutdown;
};

#endif
