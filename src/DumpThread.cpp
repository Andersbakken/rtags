#include "DumpThread.h"
#include <rct/Connection.h>
#include "RTagsClang.h"
#include "Server.h"

DumpThread::DumpThread(const std::shared_ptr<QueryMessage> &queryMessage, const Source &source, Connection *conn)
    : Thread(), mQueryFlags(queryMessage->flags()), mSource(source), mConnection(conn), mIndentLevel(0)
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

                CXSourceRange range = clang_getCursorExtent(cursor);
                CXSourceLocation rangeEnd = clang_getRangeEnd(range);
                unsigned endLine, endColumn;
                clang_getPresumedLocation(rangeEnd, 0, &endLine, &endColumn);
                if (endLine == line) {
                    message += String::format<32>(" // %d-%d, %d: ", col, endColumn, that->mIndentLevel);
                } else {
                    message += String::format<32>(" // %d-%d:%d, %d: ", col, endLine, endColumn, that->mIndentLevel);
                }
                message += RTags::cursorToString(cursor, RTags::AllCursorToStringFlags);
                message.append(" " + RTags::typeName(cursor) + " ");
                CXCursor ref = clang_getCursorReferenced(cursor);
                if (clang_equalCursors(ref, cursor)) {
                    message.append("refs self");
                } else if (!clang_equalCursors(ref, nullCursor)) {
                    message.append("refs ");
                    message.append(RTags::cursorToString(ref, RTags::AllCursorToStringFlags));
                }

                CXCursor canonical = clang_getCanonicalCursor(cursor);
                if (!clang_equalCursors(canonical, cursor) && !clang_equalCursors(canonical, nullCursor)) {
                    message.append("canonical ");
                    message.append(RTags::cursorToString(canonical, RTags::AllCursorToStringFlags));
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
    CXIndex index = clang_createIndex(0, 0);
    CXTranslationUnit translationUnit = 0;
    String clangLine;
    RTags::parseTranslationUnit(mSource.sourceFile(), mSource.toCommandLine(Source::Default), translationUnit,
                                index, 0, 0, CXTranslationUnit_DetailedPreprocessingRecord, &clangLine);
    writeToConnetion(String::format<128>("Indexed: %s => %s", clangLine.constData(), translationUnit ? "success" : "failure"));
    if (translationUnit) {
        clang_visitChildren(clang_getTranslationUnitCursor(translationUnit), DumpThread::visitor, this);
        clang_disposeTranslationUnit(translationUnit);
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
