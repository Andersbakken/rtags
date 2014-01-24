#include "DumpThread.h"
#include <rct/Connection.h>
#include "RTagsClang.h"
#include "Cpp.h"
#include "Server.h"

DumpThread::DumpThread(const QueryMessage &queryMessage, const Source &source, Connection *conn)
    : Thread(), mQueryFlags(queryMessage.flags()), mSource(source), mConnection(conn),
      mDefaultArguments(Server::instance()->options().defaultArguments), mIndentLevel(0)
{
    setAutoDelete(true);
}

static const CXSourceLocation nullLocation = clang_getNullLocation();
static const CXCursor nullCursor = clang_getNullCursor();

CXChildVisitResult DumpThread::visitor(CXCursor cursor, CXCursor, CXClientData userData)
{
    DumpThread *that = reinterpret_cast<DumpThread*>(userData);
    assert(that);
    CXSourceLocation location = clang_getCursorLocation(cursor);
    if (!clang_equalLocations(location, nullLocation)) {
        CXString file;
        unsigned line, col;
        clang_getPresumedLocation(location, &file, &line, &col);
        Path path = RTags::eatString(file);
        if (!path.isEmpty()) {
            uint32_t &fileId = that->mFiles[path];
            if (!fileId) {
                const Path resolved = path.resolved();
                fileId = Location::insertFile(resolved);
                that->mFiles[path] = that->mFiles[resolved] = fileId;
            }
            if (that->mQueryFlags & QueryMessage::DumpIncludeHeaders || fileId == that->mSource.fileId) {
                const Location loc(fileId, line, col);
                String message;
                message.reserve(256);
                if (!(that->mQueryFlags & QueryMessage::NoContext))
                    message += loc.context();
                message += String::format<32>(" // %d, %d: ", col, that->mIndentLevel);
                message += RTags::cursorToString(cursor, RTags::AllCursorToStringFlags);
                message.append(" " + RTags::typeName(cursor) + " ");
                CXCursor ref = clang_getCursorReferenced(cursor);
                if (clang_equalCursors(ref, cursor)) {
                    message.append("refs self");
                } else if (!clang_equalCursors(ref, nullCursor)) {
                    message.append("refs ");
                    message.append(RTags::cursorToString(ref, RTags::AllCursorToStringFlags));
                }
                that->writeToConnetion(message);
            }
        }
    }
    ++that->mIndentLevel;
    clang_visitChildren(cursor, DumpThread::visitor, userData);
    --that->mIndentLevel;
    return CXChildVisit_Continue;
}

void DumpThread::run()
{
    std::shared_ptr<Cpp> cpp = RTags::preprocess(mSource);
    if (!cpp) {
        writeToConnetion(String::format<128>("Failed to preprocess %s", mSource.sourceFile().constData()));
        EventLoop::mainEventLoop()->callLaterMove(std::bind((bool(Connection::*)(Message&&))&Connection::send, mConnection, std::placeholders::_1),
                                                  FinishMessage());
        return;
    }
    writeToConnetion(String::format<128>("Preprocessed %s", mSource.sourceFile().constData()));

    CXIndex index = clang_createIndex(0, 0);
    CXTranslationUnit unit = 0;
    String clangLine;
    RTags::parseTranslationUnit(mSource.sourceFile(), mSource.arguments, mDefaultArguments, unit,
                                index, 0, 0, 0, &clangLine);
    writeToConnetion(String::format<128>("Indexed: %s => %s", clangLine.constData(), unit ? "success" : "failure"));
    if (unit) {
        clang_visitChildren(clang_getTranslationUnitCursor(unit), DumpThread::visitor, this);
        clang_disposeTranslationUnit(unit);
    }

    clang_disposeIndex(index);
    EventLoop::mainEventLoop()->callLaterMove(std::bind((bool(Connection::*)(Message&&))&Connection::send, mConnection, std::placeholders::_1),
                                              FinishMessage());
}

void DumpThread::writeToConnetion(const String &message)
{
    EventLoop::mainEventLoop()->callLaterMove(std::bind((bool(Connection::*)(Message&&))&Connection::send, mConnection, std::placeholders::_1),
                                              ResponseMessage(message));
}
