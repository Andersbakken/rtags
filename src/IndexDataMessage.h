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

#ifndef IndexDataMessage_h
#define IndexDataMessage_h

#include "RTagsMessage.h"
#include "Diagnostic.h"
#include <rct/Message.h>
#include <rct/Serializer.h>
#include <rct/String.h>
#include "IndexerJob.h"

class IndexDataMessage : public RTagsMessage
{
public:
    enum { MessageId = IndexDataMessageId };

    IndexDataMessage(const std::shared_ptr<IndexerJob> &job)
        : RTagsMessage(MessageId), mParseTime(0), mKey(job->source.key()), mId(0), mIndexerJobFlags(job->flags)
    {}

    IndexDataMessage()
        : RTagsMessage(MessageId), mParseTime(0), mKey(0), mId(0), mIndexerJobFlags(0)
    {}

    void encode(Serializer &serializer) const
    {
        serializer << mProject << mParseTime << mKey << mId << mIndexerJobFlags
                   << mMessage << mFixIts << mIncludes << mDiagnostics << mFiles
                   << mDeclarations << mErrorHeaders;
    }

    void decode(Deserializer &deserializer)
    {
        deserializer >> mProject >> mParseTime >> mKey >> mId >> mIndexerJobFlags
                     >> mMessage >> mFixIts >> mIncludes >> mDiagnostics
                     >> mFiles >> mDeclarations >> mErrorHeaders;
    }

    Set<uint32_t> visitedFiles() const
    {
        Set<uint32_t> ret;
        for (Hash<uint32_t, bool>::const_iterator it = mFiles.begin(); it != mFiles.end(); ++it) {
            if (it->second)
                ret.insert(it->first);
        }
        return ret;
    }

    Set<uint32_t> blockedFiles() const
    {
        Set<uint32_t> ret;
        for (Hash<uint32_t, bool>::const_iterator it = mFiles.begin(); it != mFiles.end(); ++it) {
            if (!it->second)
                ret.insert(it->first);
        }
        return ret;
    }

    uint32_t fileId() const
    {
        uint32_t fileId, buildRootId;
        Source::decodeKey(mKey, fileId, buildRootId);
        return fileId;
    }

    const Path &project() const { return mProject; }
    void setProject(const Path &project) { mProject = project; }

    uint64_t id() const { return mId; }
    void setId(uint64_t id) { mId = id; }

    uint64_t parseTime() const { return mParseTime; }
    void setParseTime(uint64_t parseTime) { mParseTime = parseTime; }

    uint32_t indexerJobFlags() const { return mIndexerJobFlags; }
    void setIndexerJobFlags(uint32_t flags) { mIndexerJobFlags = flags; }

    uint64_t key() const { return mKey; }
    void setKey(uint64_t key) { mKey = key; }

    const String &message() const { return mMessage; }
    void setMessage(const String &msg) { mMessage = msg; }

    FixIts &fixIts() { return mFixIts; }
    Diagnostics &diagnostics() { return mDiagnostics; }
    Includes &includes() { return mIncludes; }
    Declarations &declarations() { return mDeclarations; }
    Hash<uint32_t, bool> &files() { return mFiles; }
    Set<uint32_t> &errorHeaders() { return mErrorHeaders; }
private:
    Path mProject;
    uint64_t mParseTime, mKey, mId;
    uint32_t mIndexerJobFlags; // indexerjobflags
    String mMessage; // used as output for dump when flags & Dump
    FixIts mFixIts;
    Diagnostics mDiagnostics;
    Includes mIncludes;
    Declarations mDeclarations; // function declarations and forward declaration
    Hash<uint32_t, bool> mFiles;
    Set<uint32_t> mErrorHeaders;
};

#endif
