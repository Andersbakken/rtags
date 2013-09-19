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

#ifndef IndexerMessage_h
#define IndexerMessage_h

#include <rct/Message.h>
#include <rct/String.h>
#include "ClientMessage.h"

class IndexerMessage : public ClientMessage
{
public:
    enum { MessageId = IndexerMessageId };

    IndexerMessage(uint32_t fileId, const String &project, uint64_t parseTime, SymbolMap &&symbols,
                   ReferenceMap &&references, SymbolNameMap &&symbolNames, DependencyMap &&dependencies,
                   String &&message, FixItMap &&fixIts, String &&xmlDiagnostics,
                   Map<uint32_t, bool> &&visited, int parseDuration, int visitDuration, String &&logOutput)
        : ClientMessage(MessageId), mFileId(fileId), mProject(project), mParseTime(parseTime),
          mSymbols(symbols), mReferences(references), mSymbolNames(symbolNames),
        mDependencies(dependencies), mMessage(message), mFixIts(fixIts),
        mXmlDiagnostics(xmlDiagnostics), mVisited(visited),
        mParseDuration(parseDuration), mVisitDuration(visitDuration),
        mLogOutput(logOutput)
    {
    }

    IndexerMessage()
        : ClientMessage(MessageId), mFileId(0), mParseTime(0), mParseDuration(-1), mVisitDuration(-1)
    {}

    void encode(Serializer &serializer) const
    {
        serializer << mFileId << mParseTime << mSymbols << mReferences
                   << mSymbolNames << mDependencies << mMessage << mFixIts
                   << mXmlDiagnostics << mVisited << mParseDuration << mVisitDuration;
    }
    void decode(Deserializer &deserializer)
    {
        deserializer >> mFileId >> mParseTime >> mSymbols >> mReferences
                     >> mSymbolNames >> mDependencies >> mMessage >> mFixIts
                     >> mXmlDiagnostics >> mVisited >> mParseDuration >> mVisitDuration;
    }
    uint32_t fileId() const { return mFileId; }
    uint64_t parseTime() const { return mParseTime; }
    int parseDuration() const { return mParseDuration; }
    int visitDuration() const { return mVisitDuration; }

    // non-const
    SymbolMap &&takeSymbols() { return std::move(mSymbols); }
    ReferenceMap &&takeReferences() { return std::move(mReferences); }
    SymbolNameMap &&takeSymbolNames() { return std::move(mSymbolNames); }
    DependencyMap &&takeDependencies() { return std::move(mDependencies); }
    String &&takeMessage() { return std::move(mMessage); }
    FixItMap &&takeFixIts() { return std::move(mFixIts); }
    String &&takeXmlDiagnostics() { return std::move(mXmlDiagnostics); }
    Map<uint32_t, bool> &&takeVisited() { return std::move(mVisited); }
    String &&takeProject() { return std::move(mProject); }
private:
    uint32_t mFileId;
    String mProject;
    uint64_t mParseTime;
    SymbolMap mSymbols;
    ReferenceMap mReferences;
    SymbolNameMap mSymbolNames;
    DependencyMap mDependencies;
    String mMessage;
    FixItMap mFixIts;
    String mXmlDiagnostics;
    Map<uint32_t, bool> mVisited;
    int mParseDuration;
    int mVisitDuration;
    String mLogOutput;
};

#endif
