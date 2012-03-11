#include "FollowLocationJob.h"
#include "Tools.h"

FollowLocationJob::FollowLocationJob(const QByteArray&fn, int i, int l, int c)
    : fileName(fn), id(i), line(l), col(c)
{
}

FollowLocationJob::~FollowLocationJob()
{
}

void FollowLocationJob::run()
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
            qDebug("follow: no unit for %s", fileName.constData());
            emit complete(id, QList<QByteArray>());
            return;
        }
    }

    CXFile file = clang_getFile(data->unit, fileName.constData());
    CXSourceLocation loc = clang_getLocation(data->unit, file, line, col);
    CXCursor cursor = clang_getCursor(data->unit, loc);
    CXCursorKind cursorKind = clang_getCursorKind(cursor);
    CXCursor ref = clang_getCursorReferenced(cursor);
    CXCursorKind refKind = clang_getCursorKind(ref);
    qDebug() << "cursor" << cursorKind << "ref" << refKind;
    debugCursor(cursor);
    debugCursor(ref);
    qDebug() << "---";
    bool shouldFindDef = false;
    switch (refKind) {
    case CXCursor_StructDecl:
    case CXCursor_ClassDecl:
        shouldFindDef = true;
        break;
    default:
        break;
    }
    if (clang_equalCursors(cursor, ref) || shouldFindDef) {
        qDebug() << "I am myself or we should be on a definition";
        if (!clang_isCursorDefinition(ref)) {
            qDebug() << "not an outright definition";
            ref = clang_getCursorDefinition(cursor);
        }
        if (!clang_isCursorDefinition(ref)) {
            qDebug() << "no definition found, trying to read one from leveldb";
            CXString usr = clang_getCursorUSR(cursor);
            const char* cusr = clang_getCString(usr);
            if (!strlen(cusr)) {
                clang_disposeString(usr);
                usr = clang_getCursorUSR(ref);
                cusr = clang_getCString(usr);
                if (!strlen(cusr)) {
                    clang_disposeString(usr);
                    qDebug() << "no USR found, bailing out";
                    emit complete(id, QList<QByteArray>());
                    return;
                }
            }

            leveldb::DB* db = 0;
            QByteArray dbname = Database::databaseName(Database::Definition);
            leveldb::Status status = leveldb::DB::Open(leveldb::Options(), dbname.constData(), &db);
            if (!status.ok()) {
                qWarning("no definition db!");
                clang_disposeString(usr);
                emit complete(id, QList<QByteArray>());
                return;
            }
            std::string value;
            db->Get(leveldb::ReadOptions(), cusr, &value);
            clang_disposeString(usr);
            delete db;
            if (value.empty()) {
                qDebug() << "no definition resource found, bailing out";
                emit complete(id, QList<QByteArray>());
                return;
            }
            QByteArray bvalue = QByteArray::fromRawData(value.c_str(), value.size());
            QList<QByteArray> defs = bvalue.split('\n');
            emit complete(id, defs);
            return;
        }
    }
    loc = clang_getCursorLocation(ref);

    CXFile rfile;
    unsigned int rrow, rcol, roff;
    clang_getSpellingLocation(loc, &rfile, &rrow, &rcol, &roff);
    CXString unitfn = clang_getFileName(rfile);

    qDebug() << "followed to" << clang_getCString(unitfn) << rrow << rcol << roff;
    QByteArray qfn(Path::resolved(clang_getCString(unitfn)));
    qfn += ":" + QByteArray::number(rrow)
        + ":" + QByteArray::number(rcol)
        + ":" + QByteArray::number(roff);

    clang_disposeString(unitfn);

    emit complete(id, QList<QByteArray>() << qfn);
}
