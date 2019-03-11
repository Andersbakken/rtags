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

#define RTAGS_SINGLE_THREAD
#include "RPClangIndexer.h"

// #include <unistd.h>
// #if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 25)
// #include <clang-c/Documentation.h>
// #endif

// #include "Diagnostic.h"
// #include "FileMap.h"
// #include "QueryMessage.h"
#include "RClient.h"
#include "rct/Connection.h"
#include "rct/EventLoop.h"
// #include "rct/SHA256.h"
#include "RTags.h"
#include "RTagsVersion.h"
#include "VisitFileMessage.h"
#include "VisitFileResponseMessage.h"
#include "Location.h"

RPClangIndexer::RPClangIndexer()
    : mConnection(Connection::create(RClient::NumOptions))
{
    mConnection->newMessage().connect(std::bind(&RPClangIndexer::onMessage, this,
                                                std::placeholders::_1, std::placeholders::_2));
}

RPClangIndexer::~RPClangIndexer()
{
}

bool RPClangIndexer::exec(const String &data)
{
    Deserializer deserializer(data);
    uint16_t protocolVersion;
    deserializer >> protocolVersion;
    if (protocolVersion != RTags::DatabaseVersion) {
        error("Wrong protocol %d vs %d", protocolVersion, RTags::DatabaseVersion);
        return false;
    }
    String socketFile;
    uint32_t connectTimeout, connectAttempts;
    int32_t niceValue;
    Hash<uint32_t, Path> blockedFiles;
    Path serverSandboxRoot;

    Config config;

    deserializer >> serverSandboxRoot;
    Sandbox::setRoot(serverSandboxRoot);

    deserializer >> config.id;
    deserializer >> socketFile;
    deserializer >> config.project;
    uint32_t count;
    deserializer >> count;
    config.sources.resize(count);
    for (uint32_t i=0; i<count; ++i) {
        config.sources[i].decode(deserializer, Source::IgnoreSandbox);
    }
    deserializer >> config.sourceFile;
    deserializer >> config.indexerJobFlags;
    deserializer >> mVisitFileTimeout;
    deserializer >> mIndexDataMessageTimeout;
    deserializer >> connectTimeout;
    deserializer >> connectAttempts;
    deserializer >> niceValue;
    deserializer >> config.serverOpts;
    deserializer >> config.unsavedFiles;
    deserializer >> config.dataDir;
    deserializer >> config.debugLocations;
    deserializer >> blockedFiles;

    if (config.serverOpts & Server::NoRealPath) {
        Path::setRealPathEnabled(false);
    }

    if (config.serverOpts & Server::SuspendRPOnCrash) {
        extern bool suspendRPOnCrash;
        suspendRPOnCrash = true;
    }

#if 0
    while (true) {
        FILE *f = fopen((String("/tmp/stop_") + mSourceFile.fileName()).constData(), "r+");
        if (f) {
            fseek(f, 0, SEEK_END);
            fprintf(f, "Waiting ... %d\n", getpid());
            fclose(f);
            sleep(1);
        } else {
            break;
        }
    }
#endif

    if (niceValue != INT_MIN) {
        errno = 0;
        if (nice(niceValue) == -1) {
            error() << "Failed to nice rp" << Rct::strerror();
        }
    }

    Location::init(blockedFiles);
    Location::set(config.sourceFile, config.sources.front().fileId);
    while (true) {
        if (mConnection->connectUnix(socketFile, connectTimeout))
            break;
        if (!--connectAttempts) {
            error("Failed to connect to rdm on %s (%dms timeout)", socketFile.constData(), connectTimeout);
            return false;
        }
        usleep(500 * 1000);
    }

    assert(mConnection->isConnected());
    assert(mSources.front().fileId);
    return ClangIndexer::exec(std::move(config));
}

bool RPClangIndexer::send(const IndexDataMessage &msg)
{
    StopWatch sw;
    if (!mConnection->send(msg)) {
        error() << "Couldn't send IndexDataMessage" << sourceFile();
        return false;
    }
    mConnection->finished().connect(std::bind(&EventLoop::quit, EventLoop::eventLoop()));
    if (EventLoop::eventLoop()->exec(mIndexDataMessageTimeout) == EventLoop::Timeout) {
        error() << "Timed out sending IndexDataMessage" << sourceFile();
        return false;
    }
    if (getenv("RDM_DEBUG_INDEXERMESSAGE"))
        error() << "Send took" << sw.elapsed() << "for" << sourceFile();

    return true;
}

void RPClangIndexer::onMessage(const std::shared_ptr<Message> &msg, const std::shared_ptr<Connection> &/*conn*/)
{
    assert(msg->messageId() == VisitFileResponseMessage::MessageId);
    const std::shared_ptr<VisitFileResponseMessage> vm = std::static_pointer_cast<VisitFileResponseMessage>(msg);
    mVisitFileResponseMessageVisit = vm->visit();
    mVisitFileResponseMessageFileId = vm->fileId();
    assert(EventLoop::eventLoop());
    EventLoop::eventLoop()->quit();
}

Location RPClangIndexer::createLocation(const Path &sourceFile, unsigned int line, unsigned int col, bool *blockedPtr)
{
    uint32_t id = Location::fileId(sourceFile);
    Path resolved;
    if (!id) {
        bool ok;
        for (int i=0; i<4; ++i) {
            resolved = sourceFile.resolved(Path::RealPath, Path(), &ok);
            // if ok is false it means the file is gone, in case this happens
            // during a git pull or something we'll give it a couple of chances.
            if (ok)
                break;
            usleep(50000);
        }
        if (!ok)
            return Location();
        id = Location::fileId(resolved);
        if (id)
            Location::set(sourceFile, id);
    }
    assert(!resolved.contains("/../"));

    if (id) {
        if (blockedPtr) {
            IndexDataMessage &indexData = indexDataMessage();
            Hash<uint32_t, Flags<IndexDataMessage::FileFlag> >::iterator it = indexData.files().find(id);
            if (it == indexData.files().end()) {
                // the only reason we already have an id for a file that isn't
                // in the mIndexDataMessage.mFiles is that it's blocked from the outset.
                // The assumption is that we never will go and fetch a file id
                // for a location without passing blockedPtr since any reference
                // to a symbol in another file should have been preceded by that
                // header in which case we would have to make a decision on
                // whether or not to index it. This is a little hairy but we
                // have to try to optimize this process.
#ifndef NDEBUG
                if (resolved.isEmpty())
                    resolved = sourceFile.resolved();
#endif
                assert(id);
                indexData.files()[id] = IndexDataMessage::NoFileFlag;
                *blockedPtr = true;
            } else if (!it->second) {
                *blockedPtr = true;
            }
        }
        return Location(id, line, col);
    }

    ++fileIdsQueried();
    VisitFileMessage msg(resolved, project(), sources().front().fileId);

    mVisitFileResponseMessageFileId = UINT_MAX;
    mVisitFileResponseMessageVisit = false;
    mConnection->send(msg);
    StopWatch sw;
    EventLoop::eventLoop()->exec(mVisitFileTimeout);
    const int elapsed = sw.elapsed();
    fileIdsQueriedTime() += elapsed;
    switch (mVisitFileResponseMessageFileId) {
    case 0:
        return Location();
    case UINT_MAX:
        // timed out.
        if (mVisitFileResponseMessageFileId == UINT_MAX) {
            error() << "Error getting fileId for" << resolved << lastCursor()
                    << elapsed << mVisitFileTimeout;
        }
        abort();
    default:
        id = mVisitFileResponseMessageFileId;
        break;
    }
    assert(id);
    Flags<IndexDataMessage::FileFlag> &flags = indexDataMessage().files()[id];
    if (mVisitFileResponseMessageVisit) {
        flags |= IndexDataMessage::Visited;
        ++indexed();
    }
    // fprintf(mLogFile, "%s %s\n", file.second ? "WON" : "LOST", resolved.constData());

    Location::set(resolved, id);
    if (resolved != sourceFile)
        Location::set(sourceFile, id);

    if (blockedPtr)
        *blockedPtr = !mVisitFileResponseMessageVisit;
    return Location(id, line, col);
}
