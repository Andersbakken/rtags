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

#include "DumpThread.h"
#include <rct/Connection.h>
#include "RTagsClang.h"
#include "Server.h"

DumpThread::DumpThread(const std::shared_ptr<QueryMessage> &queryMessage, const Source &source, const std::shared_ptr<Connection> &conn)
    : Thread(), mQueryFlags(queryMessage->flags()), mSource(source), mConnection(conn), mIndentLevel(0), mAborted(false)
{
    setAutoDelete(true);
}

static const CXSourceLocation nullLocation = clang_getNullLocation();
static const CXCursor nullCursor = clang_getNullCursor();

CXChildVisitResult DumpThread::visitor(CXCursor cursor, CXCursor, CXClientData userData)
{
    DumpThread *that = reinterpret_cast<DumpThread*>(userData);
    assert(that);
    if (that->isAborted())
        return CXChildVisit_Break;
    CXSourceLocation location = clang_getCursorLocation(cursor);
    if (!clang_equalLocations(location, nullLocation)) {
        Flags<Location::KeyFlag> locationFlags;
        if (that->mQueryFlags & QueryMessage::NoColor)
            locationFlags |= Location::NoColor;
        CXString file;
        unsigned int line, col;
        clang_getPresumedLocation(location, &file, &line, &col);
        Path path = RTags::eatString(file);
        if (!path.isEmpty() && path != "<built-in>") {
            CXSourceRange range = clang_getCursorExtent(cursor);
            CXSourceLocation rangeEnd = clang_getRangeEnd(range);
            unsigned int endLine, endColumn;
            clang_getPresumedLocation(rangeEnd, 0, &endLine, &endColumn);
            uint32_t &fileId = that->mFiles[path];
            if (!fileId) {
                const Path resolved = path.resolved();
                fileId = Location::insertFile(resolved);
                that->mFiles[path] = that->mFiles[resolved] = fileId;
            }
            if (!(that->mQueryFlags & QueryMessage::DumpIncludeHeaders) && fileId != that->mSource.fileId) {
                return CXChildVisit_Continue;
            }

            String message;
            message.reserve(256);

            if (!(that->mQueryFlags & QueryMessage::NoContext)) {
                const Location loc(fileId, line, col);
                message = loc.context(locationFlags);
            }

            if (endLine == line) {
                message += String::format<32>(" // %d-%d, %d: ", col, endColumn, that->mIndentLevel);
            } else {
                message += String::format<32>(" // %d-%d:%d, %d: ", col, endLine, endColumn, that->mIndentLevel);
            }
            message += RTags::cursorToString(cursor, RTags::AllCursorToStringFlags);
            message.append(" " + RTags::typeName(cursor));;
            if (clang_getCursorKind(cursor) == CXCursor_VarDecl) {
                const std::shared_ptr<RTags::Auto> autoResolved = RTags::resolveAuto(cursor);
                if (autoResolved && !clang_equalCursors(autoResolved->cursor, nullCursor)) {
                    message += "auto resolves to " + RTags::cursorToString(autoResolved->cursor, RTags::AllCursorToStringFlags);
                }
            }
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

            CXCursor specialized = clang_getSpecializedCursorTemplate(cursor);
            if (!clang_equalCursors(specialized, cursor) && !clang_equalCursors(specialized, nullCursor)) {
                message.append("specialized ");
                message.append(RTags::cursorToString(specialized, RTags::AllCursorToStringFlags));
            }

            that->writeToConnetion(message);
        }
    }
    ++that->mIndentLevel;
    clang_visitChildren(cursor, DumpThread::visitor, userData);
    if (that->isAborted())
        return CXChildVisit_Break;
    --that->mIndentLevel;
    return CXChildVisit_Continue;
}

void DumpThread::run()
{
    const auto key = mConnection->disconnected().connect([this](const std::shared_ptr<Connection> &) { abort(); });

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
    mConnection->disconnected().disconnect(key);
    std::weak_ptr<Connection> conn = mConnection;
    EventLoop::mainEventLoop()->callLater([conn]() {
            if (auto c = conn.lock())
                c->finish();
        });
}

void DumpThread::writeToConnetion(const String &message)
{
    std::weak_ptr<Connection> conn = mConnection;
    EventLoop::mainEventLoop()->callLater([conn, message]() {
            if (auto c = conn.lock()) {
                c->write(message);
            }
        });
}
