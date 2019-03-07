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

#ifndef RPVEHICLE_H
#define RPVEHICLE_H

#include "rct/ThreadPool.h"
#include "rct/String.h"
#include "rct/SignalSlot.h"
#include "rct/Path.h"
#include "rct/Process.h"
#include "IndexerJob.h"
#include "JobScheduler.h"
#include "ClangIndexer.h"

class RPProcess : public Process, public Vehicle
{
public:
    RPProcess();
    virtual void kill() override;
    virtual String readAllStdOut() override;
    virtual String readAllStdErr() override;
    virtual String errorString() const override;
    virtual unsigned long long id() const override;
    virtual int returnCode() const override;
    virtual bool start(const std::shared_ptr<IndexerJob> &job) override;
};

class RPThread : public ThreadPool::Job, public Vehicle, public ClangIndexer
{
public:
    RPThread();
    virtual void kill() override;
    virtual void run() override;
    virtual String readAllStdOut() override;
    virtual String readAllStdErr() override;
    virtual String errorString() const override;
    virtual unsigned long long id() const override;
    virtual int returnCode() const override;
    virtual bool start(const std::shared_ptr<IndexerJob> &job) override;
protected:
    virtual Location createLocation(const Path &sourceFile, unsigned int line, unsigned int col, bool *blockedPtr) override;
    virtual bool send(const std::shared_ptr<IndexDataMessage> &message) override;
    virtual bool interrupt() override;
private:
    struct FileInfo {
        uint32_t fileId { 0 };
        enum Status {
            Lost,
            Won,
            Pending
        } status { Pending };
    };
    Hash<Path, FileInfo> mPaths;
    const unsigned long long mId { 0 };
    std::shared_ptr<IndexerJob> mJob;
    std::shared_ptr<Project> mProject;
    uint32_t mSourceFileId { 0 };
    String mErrorString;
    std::mutex mMutex;
    bool mKilled { false };
};

#endif
