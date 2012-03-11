#include "CursorInfoJob.h"
#include <clang-c/Index.h>
#include <Path.h>
#include <Tools.h>

CursorInfoJob::CursorInfoJob(const QByteArray&fn, int i, int l, int c)
    : fileName(fn), id(i), line(l), col(c)
{
}

CursorInfoJob::~CursorInfoJob()
{
}

static CXChildVisitResult memberVisitor(CXCursor cursor, CXCursor, CXClientData userData)
{
    switch (clang_getCursorKind(cursor)) {
    case CXCursor_CXXMethod:
    case CXCursor_FieldDecl:
        reinterpret_cast<QList<QByteArray> *>(userData)->append(eatString(clang_getCursorSpelling(cursor)));
        break;
    default:
        break;
    }
    return CXChildVisit_Continue;
}

static inline bool hasMembers(CXCursor cursor)
{
    switch (clang_getCursorKind(cursor)) {
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
        return true;
    default:
        break;
    }
    return false;
}

void CursorInfoJob::run()
{
    CachedUnit locker(fileName, UnitCache::AST | UnitCache::Memory);
    UnitCache::Unit* data = locker.unit();
    if (!data) {
        FirstUnitData first;
        first.fileName = fileName;
        visitIncluderFiles(fileName, visitFindFirstUnit, &first);
        if (first.data) {
            locker.adopt(first.data);
            data = first.data;
        } else {
            qDebug("cursorInfo: no unit for %s", fileName.constData());
            emit complete(id, QList<QByteArray>());
            return;
        }
    }

    CXFile file = clang_getFile(data->unit, fileName.constData());
    CXSourceLocation loc = clang_getLocation(data->unit, file, line, col);
    CXCursor cursor = clang_getCursor(data->unit, loc);
    QList<QByteArray> ret;
    if (!clang_isInvalid(clang_getCursorKind(cursor))) {
        bool go = hasMembers(cursor);
        if (!go) {
            cursor = clang_getCursorReferenced(cursor);
            go = hasMembers(cursor);
        }
        if (go)
            clang_visitChildren(cursor, memberVisitor, &ret);
    }
    emit complete(id, ret);
}
