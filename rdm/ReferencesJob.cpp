#include "Database.h"
#include "ReferencesJob.h"
#include "Server.h"
#include "Rdm.h"
#include "CursorInfo.h"

ReferencesJob::ReferencesJob(int i, const Location &loc, unsigned fl)
    : Job(i, QueryJobPriority), symbolName(QByteArray()), flags(fl)
{
    locations.insert(loc);
}

ReferencesJob::ReferencesJob(int i, const QByteArray &sym, unsigned fl)
    : Job(i, QueryJobPriority), symbolName(sym), flags(fl)
{
}

void ReferencesJob::execute()
{
    if (!symbolName.isEmpty()) {
        ScopedDB db = Server::instance()->db(Server::SymbolName, ScopedDB::Read);
        locations = db->value<QSet<Location> >(symbolName);
        if (locations.isEmpty()) {
            return;
        }
    }
    const bool excludeDefsAndDecls = !(flags & QueryMessage::IncludeDeclarationsAndDefinitions);
    ScopedDB db = Server::instance()->db(Server::Symbol, ScopedDB::Read);
    const unsigned keyFlags = QueryMessage::keyFlags(flags);
    foreach(const Location &location, locations) {
        if (isAborted())
            return;
        QSet<Location> refs;
        QSet<Location> filtered;

        Location realLoc;
        CursorInfo cursorInfo = Rdm::findCursorInfo(db, location, &realLoc);
        qDebug() << "getting references at" << realLoc << Rdm::eatString(clang_getCursorKindSpelling(cursorInfo.kind))
                 << cursorInfo.references << cursorInfo.target
                 << clang_isReference(cursorInfo.kind)
                 << (cursorInfo.kind >= CXCursor_FirstExpr && cursorInfo.kind <= CXCursor_LastExpr);
        if (clang_isReference(cursorInfo.kind) || (cursorInfo.kind >= CXCursor_FirstExpr && cursorInfo.kind <= CXCursor_LastExpr)) {
            filtered.insert(cursorInfo.target);
            cursorInfo = Rdm::findCursorInfo(db, cursorInfo.target);
        } else if (excludeDefsAndDecls) {
            filtered.insert(realLoc);
        } else {
            refs.insert(realLoc);
        }
        if (cursorInfo.isValid()) {
            if (cursorInfo.target.isValid()) {
                if (excludeDefsAndDecls) {
                    filtered.insert(cursorInfo.target);
                } else {
                    refs.insert(cursorInfo.target);
                }
            }
            foreach(const Location &l, cursorInfo.references) {
                if (!excludeDefsAndDecls || !filtered.contains(l)) {
                    refs.insert(l);
                }
            }
            if (cursorInfo.target.isValid() && cursorInfo.kind != CXCursor_VarDecl) {
                cursorInfo = Rdm::findCursorInfo(db, cursorInfo.target);
                foreach(const Location &l, cursorInfo.references) {
                    if (!excludeDefsAndDecls || !filtered.contains(l)) {
                        refs.insert(l);
                    }
                }
            }
        }
        QList<Location> sorted = refs.toList();
        if (flags & QueryMessage::ReverseSort) {
            qSort(sorted.begin(), sorted.end(), qGreater<Location>());
        } else {
            qSort(sorted);
        }
        foreach (const Location &loc, sorted) {
            write(loc.key(keyFlags));
        }
    }
}
