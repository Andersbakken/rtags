#include "FollowLocationJob.h"
#include "Rdm.h"

FollowLocationJob::FollowLocationJob(int i, const RTags::Location &loc)
    : id(i), location(loc)
{
}

FollowLocationJob::~FollowLocationJob()
{
}

void FollowLocationJob::run()
{
    leveldb::DB* db = 0;
    leveldb::Options options;
    const QByteArray name = Database::databaseName(Database::SymbolName);
    if (name.isEmpty()) {
        emit complete(id, QList<QByteArray>());
        return;
    }

    leveldb::Status status = leveldb::DB::Open(options, name.constData(), &db);
    if (!status.ok()) {
        emit complete(id, QList<QByteArray>());
        return;
    }
    Q_ASSERT(db);

    const leveldb::ReadOptions readopts;
    leveldb::Iterator* it = db->NewIterator(readopts);
    const QByteArray needle = location.key();
    it->Seek(needle.constData());
    QList<QByteArray> list;
    if (it->Valid()) {
        const leveldb::Slice k = it->key();
        Rdm::CursorInfo cursorInfo;
        const QByteArray key = QByteArray::fromRawData(k.data(), k.size());
        bool found = (key == needle);
        if (!found) {
            const RTags::Location loc = RTags::Location::fromKey(key);
            if (location.path == loc.path) {
                const int off = location.offset - loc.offset;
                cursorInfo = Rdm::readValue<Rdm::CursorInfo>(it);
                if (cursorInfo.symbolLength > off) {
                    found = true;
                }
            }
        }
        if (found) {
            if (!cursorInfo.symbolLength)
                cursorInfo = Rdm::readValue<Rdm::CursorInfo>(it);
            if (!cursorInfo.target.isNull())
                list.append(cursorInfo.target.key());
        }
    }
    delete it;
    emit complete(id, list);


    // SymbolNameHash::iterator it = mSymbolNames.begin();
    // const SymbolNameHash::const_iterator end = mSymbolNames.end();
    // while (it != end) {
    //     const char *key = it.key().constData();
    //     const QSet<RTags::Location> added = it.value();
    //     QSet<RTags::Location> current = Rdm::readValue<QSet<RTags::Location> >(db, key);
    //     const int oldSize = current.size();
    //     current += added;
    //     if (current.size() != oldSize)
    //         Rdm::writeValue<QSet<RTags::Location> >(&batch, key, current);
    //     ++it;
    // }


//     QByteArray qfn;
//     unsigned int rrow, rcol, roff;
//     {
//         CachedUnit locker(location.path, UnitCache::AST | UnitCache::Memory);
//         UnitCache::Unit* data = locker.unit();
//         if (!data) {
//             Rdm::FirstUnitData first;
//             first.fileName = location.path;
//             Rdm::visitIncluderFiles(location.path, Rdm::visitFindFirstUnit, &first);
//             if (first.data) {
//                 locker.adopt(first.data);
//                 data = first.data;
//             } else {
//                 warning("follow: no unit for %s", location.path.constData());
//                 emit complete(id, QList<QByteArray>());
//                 return;
//             }
//         }

//         CXFile file = clang_getFile(data->unit, location.path.constData());
//         CXSourceLocation loc;
//         if (location.offset != -1) {
//             loc = clang_getLocationForOffset(data->unit, file, location.offset);
//         } else {
//             loc = clang_getLocation(data->unit, file, location.line, location.column);
//         }
//         CXCursor cursor = clang_getCursor(data->unit, loc);
//         CXCursorKind cursorKind = clang_getCursorKind(cursor);
//         CXCursor ref = clang_getCursorReferenced(cursor);
//         CXCursorKind refKind = clang_getCursorKind(ref);
//         log(1) << "cursor" << cursorKind << "ref" << refKind;
//         Rdm::debugCursor(cursor);
//         Rdm::debugCursor(ref);
//         log(1) << "---";
//         bool shouldFindDef = false;
//         switch (refKind) {
//         case CXCursor_StructDecl:
//         case CXCursor_ClassDecl:
//             shouldFindDef = true;
//             break;
//         default:
//             break;
//         }
//         if (clang_equalCursors(cursor, ref) || shouldFindDef) {
//             log(1) << "I am myself or we should be on a definition";
//             if (!clang_isCursorDefinition(ref)) {
//                 log(1) << "not an outright definition";
//                 ref = clang_getCursorDefinition(cursor);
//             }
//             if (!clang_isCursorDefinition(ref)) {
//                 log(1) << "no definition found, trying to read one from leveldb";
//                 CXString usr = clang_getCursorUSR(cursor);
//                 const char* cusr = clang_getCString(usr);
//                 if (!strlen(cusr)) {
//                     clang_disposeString(usr);
//                     usr = clang_getCursorUSR(ref);
//                     cusr = clang_getCString(usr);
//                     if (!strlen(cusr)) {
//                         clang_disposeString(usr);
//                         log(1) << "no USR found, bailing out";
//                         emit complete(id, QList<QByteArray>());
//                         return;
//                     }
//                 }

//                 const QList<QByteArray> defs = lookupUsr(cusr);
//                 clang_disposeString(usr);
//                 emit complete(id, defs);
//                 return;
//             }
//         }
//         loc = clang_getCursorLocation(ref);

//         CXFile rfile;
//         clang_getSpellingLocation(loc, &rfile, &rrow, &rcol, &roff);
//         if (rrow == 0 && rcol == 0) { // try to get the USR and look up in leveldb
//             log(1) << "no location at reference, trying USR";
//             CXString usr = clang_getCursorUSR(ref);
//             const char* cusr = clang_getCString(usr);
//             if (!strlen(cusr)) {
//                 warning() << "no USR for reference, bailing out";
//                 clang_disposeString(usr);
//                 emit complete(id, QList<QByteArray>());
//                 return;
//             }
//             const QList<QByteArray> defs = lookupUsr(cusr);
//             clang_disposeString(usr);
//             emit complete(id, defs);
//             return;
//         }
//         qfn = Rdm::makeLocation(ref, 0);
//         log(1) << "followed to" << qfn;
//     }

//     emit complete(id, QList<QByteArray>() << qfn);
}
