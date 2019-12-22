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

#ifndef IndexDataMessage_h
#define IndexDataMessage_h

#include "Diagnostic.h"
#include "IndexerJob.h"
#include "rct/Flags.h"
#include "rct/Serializer.h"
#include "rct/String.h"
#include "RTagsMessage.h"

class IndexDataMessage : public RTagsMessage
{
public:
    enum { MessageId = IndexDataMessageId };

    IndexDataMessage(const std::shared_ptr<IndexerJob> &job)
        : RTagsMessage(MessageId), mParseTime(0), mId(0), mIndexerJobFlags(job->flags), mBytesWritten(0)
    {}

    IndexDataMessage()
        : RTagsMessage(MessageId), mParseTime(0), mId(0), mBytesWritten(0)
    {}

    void encode(Serializer &serializer) const override;
    void decode(Deserializer &deserializer) override;

    enum Flag {
        None = 0x0,
        ParseFailure = 0x1,
        UsedPCH = 0x2
    };
    Flags<Flag> flags() const { return mFlags; }
    void setFlags(Flags<Flag> f) { mFlags = f; }
    void setFlag(Flag flag, bool on = true) { mFlags.set(flag, on); }

    Set<uint32_t> visitedFiles() const
    {
        Set<uint32_t> ret;
        for (const auto &it : mFiles) {
            if (it.second & Visited)
                ret.insert(it.first);
        }
        return ret;
    }

    Set<uint32_t> blockedFiles() const
    {
        Set<uint32_t> ret;
        for (const auto &it : mFiles) {
            if (!(it.second & Visited))
                ret.insert(it.first);
        }
        return ret;
    }

    const Path &project() const { return mProject; }
    void setProject(const Path &p) { mProject = p; }

    uint64_t id() const { return mId; }
    void setId(uint64_t i) { mId = i; }

    uint64_t parseTime() const { return mParseTime; }
    void setParseTime(uint64_t time) { mParseTime = time; }

    Flags<IndexerJob::Flag> indexerJobFlags() const { return mIndexerJobFlags; }
    void setIndexerJobFlags(Flags<IndexerJob::Flag> flags) { mIndexerJobFlags = flags; }

    const String &message() const { return mMessage; }
    void setMessage(const String &msg) { mMessage = msg; }
    void setMessage(String &&msg) { mMessage = std::move(msg); }

    FixIts &fixIts() { return mFixIts; }
    Diagnostics &diagnostics() { return mDiagnostics; }
    Includes &includes() { return mIncludes; }
    enum FileFlag {
        NoFileFlag = 0x0,
        Visited = 0x1
    };
    Hash<uint32_t, Flags<FileFlag> > &files() { return mFiles; }
    const Hash<uint32_t, Flags<FileFlag> > &files() const { return mFiles; }

    size_t bytesWritten() const { return mBytesWritten; }
    void setBytesWritten(size_t bytes) { mBytesWritten = bytes; }

    void clear()
    {
        clearCache();
        mProject.clear();
        mParseTime = 0;
        mId = 0;
        mIndexerJobFlags.clear();
        mMessage.clear();
        mFixIts.clear();
        mDiagnostics.clear();
        mIncludes.clear();
        mFiles.clear();
        mFlags.clear();
        mBytesWritten = 0;
    }
private:
    Path mProject;
    uint64_t mParseTime, mId;
    Flags<IndexerJob::Flag> mIndexerJobFlags; // indexerjobflags
    String mMessage; // used as output for dump when flags & Dump
    FixIts mFixIts;
    Diagnostics mDiagnostics;
    Includes mIncludes;
    Hash<uint32_t, Flags<FileFlag> > mFiles;
    Flags<Flag> mFlags;
    size_t mBytesWritten;
};

RCT_FLAGS(IndexDataMessage::Flag);
RCT_FLAGS(IndexDataMessage::FileFlag);

inline void IndexDataMessage::encode(Serializer &serializer) const
{
    serializer << mProject << mParseTime << mId << mIndexerJobFlags << mMessage
               << mFixIts << mIncludes << mDiagnostics << mFiles << mFlags << mBytesWritten;
}

inline void IndexDataMessage::decode(Deserializer &deserializer)
{
    deserializer >> mProject >> mParseTime >> mId >> mIndexerJobFlags >> mMessage
                 >> mFixIts >> mIncludes >> mDiagnostics >> mFiles >> mFlags >> mBytesWritten;
}

#endif
