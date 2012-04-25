#ifndef Tools_h
#define Tools_h

#include <QByteArray>
#include <clang-c/Index.h>
#include <Path.h>
#include <QDebug>
#include "Server.h"
#include <leveldb/db.h>
#include <leveldb/write_batch.h>
#include <RTags.h>

namespace Rdm {
enum { DatabaseVersion = 2 };
enum ReferenceType {
    NormalReference,
    MemberFunction,
    GlobalFunction
};
QByteArray eatString(CXString str);
QByteArray cursorToString(CXCursor cursor);
static inline bool isSystem(const char *str)
{
    bool system = !strncmp("/usr/", str, 5);
#ifdef Q_OS_BSD4
    if (system && !strncmp("home/", str + 5, 5))
        system = false;
#endif
#ifdef CLANG_RUNTIME_INCLUDE
    static const int clangRuntimeIncludeLen = strlen(CLANG_RUNTIME_INCLUDE);
    if (!strncmp(CLANG_RUNTIME_INCLUDE, str, clangRuntimeIncludeLen))
        system = true;
#endif
    
    return system;
}

struct CursorInfo {
    CursorInfo() : symbolLength(0), kind(CXCursor_FirstInvalid) {}
    bool isNull() const { return symbolLength; }
    void clear()
    {
        symbolLength = 0;
        kind = CXCursor_FirstInvalid;
        target.clear();
        references.clear();
#ifdef QT_DEBUG
        loc.clear();
        symbolName.clear();
#endif
    }
    int symbolLength;
    CXCursorKind kind;
    RTags::Location target;
    QSet<RTags::Location> references;
#ifdef QT_DEBUG
    RTags::Location loc;
    QByteArray symbolName;
#endif
    bool dirty(const QSet<Path> &paths)
    {
        bool changed = false;
        if (paths.contains(target.path)) {
            changed = true;
            target.clear();
        }

        QSet<RTags::Location>::iterator it = references.begin();
        while (it != references.end()) {
            if (paths.contains((*it).path)) {
                changed = true;
                it = references.erase(it);
            } else {
                ++it;
            }
        }
        return changed;
    }
    bool unite(const CursorInfo &other)
    {
        if (kind == CXCursor_InvalidFile)
            kind = other.kind;
        if (!other.symbolLength) {
            // error() << "other full of shit"
            //         << eatString(clang_getCursorKindSpelling(other.kind))
            //         << other.target
            //         << other.references
            //         << "me"
            //         << eatString(clang_getCursorKindSpelling(kind))
            //         << symbolLength;
            return true;
        }
        if (!symbolLength) {
#ifdef QT_DEBUG
            if (!target.isNull())
                warning() << "About to assert" << target << loc << symbolName;
            if (!references.isEmpty())
                warning() << "About to assert" << references << loc << symbolName;
#endif
            // Q_ASSERT(target.isNull());
            // Q_ASSERT(references.isEmpty());
            *this = other;
            return true;
        }
        if (symbolLength != other.symbolLength) {
            warning() << "something wrong here. SymbolLength" << symbolLength << "other.symbolLength" << other.symbolLength
                      << "target" << target << "other.target" << other.target
                      << "references" << references << "other.references" << other.references
                      << "kind" << kind << "other.kind" << other.kind
#ifdef QT_DEBUG
                      << "location" << loc << "other.location" << other.loc
                      << "symbolName" << symbolName << "other.symbolName" << other.symbolName
#endif
                ;
        }
        bool changed = false;
        if (!other.target.isNull() && target != other.target) {
#ifdef QT_DEBUG
            if (!target.isNull()) {
                switch (kind) {
                case CXCursor_TypeRef:
                case CXCursor_NamespaceRef:
                case CXCursor_MacroExpansion:
                case CXCursor_TemplateRef:
                case CXCursor_CXXBaseSpecifier:
                case CXCursor_UnexposedExpr:
                case CXCursor_CallExpr: // don't like this one
                    break;
                case CXCursor_VarDecl:
                case CXCursor_CXXMethod:
                    if (target.path.contains("moc_") || target.path.contains(".moc"))
                        break;
                    // fallthrough
                default:
                    warning() << "overwrote target for" << loc << "from" << target << "to" << other.target
                              << "symbolName" << symbolName
                              << Rdm::eatString(clang_getCursorKindSpelling(kind));
                    break;
                }
            }
#endif
            target = other.target;
            changed = true;
        }
        const int oldSize = references.size();
        references.unite(other.references);
        return changed || oldSize != references.size();
    }
};

static inline QDataStream &operator<<(QDataStream &ds, const CursorInfo &ci)
{
    ds << ci.symbolLength << ci.target << ci.references << static_cast<quint32>(ci.kind);
    return ds;
}

static inline QDataStream &operator>>(QDataStream &ds, CursorInfo &ci)
{
    quint32 kind;
    ds >> ci.symbolLength >> ci.target >> ci.references >> kind;
    ci.kind = static_cast<CXCursorKind>(kind);
    return ds;
}

static inline bool contains(leveldb::DB *db, const char *key)
{
    std::string str;
    return db->Get(leveldb::ReadOptions(), key, &str).ok();
}

template <typename T> T readValue(leveldb::DB *db, const char *key, bool *ok = 0)
{
    T t;
    std::string value;
    const leveldb::Status s = db->Get(leveldb::ReadOptions(), key, &value);
    if (!value.empty()) {
        const QByteArray v = QByteArray::fromRawData(value.c_str(), value.length());
        QDataStream ds(v);
        ds >> t;
    }
    if (ok)
        *ok = s.ok();
    return t;
}

template <typename T> T readValue(leveldb::Iterator *it)
{
    T t;
    leveldb::Slice value = it->value();
    const QByteArray v = QByteArray::fromRawData(value.data(), value.size());
    if (!v.isEmpty()) {
        QDataStream ds(v);
        ds >> t;
    }
    return t;
}

template <typename T> void writeValue(leveldb::WriteBatch *batch, const char *key, const T &t)
{
    Q_ASSERT(batch);
    QByteArray out;
    {
        QDataStream ds(&out, QIODevice::WriteOnly);
        ds << t;
    }
    batch->Put(key, leveldb::Slice(out.constData(), out.size()));
}

template <typename T> void writeValue(leveldb::DB *db, const char *key, const T &t)
{
    Q_ASSERT(db);
    Q_ASSERT(key);
    QByteArray out;
    {
        QDataStream ds(&out, QIODevice::WriteOnly);
        ds << t;
    }
    db->Put(leveldb::WriteOptions(), leveldb::Slice(key, strlen(key)),
            leveldb::Slice(out.constData(), out.size()));
}

CursorInfo findCursorInfo(leveldb::DB *db, const RTags::Location &key);
}


#endif
