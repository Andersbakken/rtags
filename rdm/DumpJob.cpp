#include "DumpJob.h"
#include <clang-c/Index.h>
#include <Tools.h>

DumpJob::DumpJob(const QByteArray& fn, int i)
    : fileName(fn), id(i)
{
}

struct DumpUserData {
    QList<QByteArray> lines;
    int indent;
};

static CXChildVisitResult dumpVisitor(CXCursor cursor, CXCursor, CXClientData userData)
{
    DumpUserData *dump = reinterpret_cast<DumpUserData*>(userData);
    QByteArray line(dump->indent * 2, ' ');
    line += cursorToString(cursor);
    CXCursor ref = clang_getCursorReferenced(cursor);
    if (!clang_equalCursors(cursor, ref) && !clang_isInvalid(clang_getCursorKind(ref))) {
        line += " => " + cursorToString(ref);
    }
    dump->lines.append(line);
    ++dump->indent;
    clang_visitChildren(cursor, dumpVisitor, dump);
    --dump->indent;
    return CXChildVisit_Continue;
}

void DumpJob::run()
{
    DumpUserData user = { QList<QByteArray>(), 0 };
    CachedUnit unit(fileName);
    if (unit.unit()) {
        clang_visitChildren(clang_getTranslationUnitCursor(unit.unit()->unit), dumpVisitor, &user);
    } else {
        qWarning("Can't load unit for %s", fileName.constData());
    }
    emit complete(id, user.lines);
}
