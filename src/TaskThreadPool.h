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

#ifndef TaskThreadPool_h
#define TaskThreadPool_h

#include <functional>
#include <memory>
#include <thread>
#include <vector>
#include <mutex>
#include <condition_variable>
#include <deque>
#include <atomic>

class TaskThreadPool
{
public:
    using Task = std::function<void()>;

    explicit TaskThreadPool(size_t numThreads = std::thread::hardware_concurrency())
        : mShutdown(false)
    {
        mWorkers.reserve(numThreads);
        for (size_t i = 0; i < numThreads; ++i) {
            mWorkers.emplace_back([this] { workerThread(); });
        }
    }

    ~TaskThreadPool()
    {
        shutdown();
    }

    void enqueue(Task task)
    {
        {
            std::unique_lock<std::mutex> lock(mMutex);
            if (mShutdown)
                return;
            mTasks.push_back(std::move(task));
        }
        mCondition.notify_one();
    }

    void shutdown()
    {
        {
            std::unique_lock<std::mutex> lock(mMutex);
            if (mShutdown)
                return;
            mShutdown = true;
        }
        mCondition.notify_all();

        for (auto &worker : mWorkers) {
            if (worker.joinable())
                worker.join();
        }
        mWorkers.clear();
    }

    size_t pendingTasks() const
    {
        std::unique_lock<std::mutex> lock(mMutex);
        return mTasks.size();
    }

private:
    void workerThread()
    {
        while (true) {
            Task task;
            {
                std::unique_lock<std::mutex> lock(mMutex);
                mCondition.wait(lock, [this] { return mShutdown || !mTasks.empty(); });

                if (mShutdown && mTasks.empty())
                    return;

                if (!mTasks.empty()) {
                    task = std::move(mTasks.front());
                    mTasks.pop_front();
                }
            }

            if (task) {
                try {
                    task();
                } catch (...) {
                    // Log error but don't crash the worker thread
                }
            }
        }
    }

    std::vector<std::thread> mWorkers;
    mutable std::mutex mMutex;
    std::condition_variable mCondition;
    std::deque<Task> mTasks;
    std::atomic<bool> mShutdown;
};

#endif
