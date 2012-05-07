#ifndef CursorInfo_h
#define CursorInfo_h

#include <QByteArray>
#include <clang-c/Index.h>
#include <Path.h>
#include <QDebug>
#include <RTags.h>
#include "Location.h"
#include "Rdm.h"

class CursorInfo
{
public:
    CursorInfo() : symbolLength(0), kind(CXCursor_FirstInvalid) {}
    bool isNull() const { return !symbolLength; }
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

    bool dirty(const QSet<quint32> &fileIds)
    {
        bool changed = false;
        if (fileIds.contains(target.fileId())) {
            changed = true;
            target.clear();
        }

        QSet<Location>::iterator it = references.begin();
        while (it != references.end()) {
            if (fileIds.contains((*it).fileId())) {
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
        bool changed = false;
        if (kind == CXCursor_InvalidFile) {
            kind = other.kind;
            changed = true;
        }
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

    int symbolLength; // this is just the symbol name e.g. foo
    QByteArray symbolName; // this is fully qualified Foobar::Barfoo::foo
    CXCursorKind kind;
    Location target;
    QSet<Location> references;
#ifdef QT_DEBUG
    Location loc;
#endif
};

static inline QDataStream &operator<<(QDataStream &ds, const CursorInfo &ci)
{
    ds << ci.symbolLength << ci.target << ci.references << static_cast<quint32>(ci.kind)
       << ci.symbolName;
    return ds;
}

static inline QDataStream &operator>>(QDataStream &ds, CursorInfo &ci)
{
    quint32 kind;
    ds >> ci.symbolLength >> ci.target >> ci.references >> kind >> ci.symbolName;
    ci.kind = static_cast<CXCursorKind>(kind);
    return ds;
}

#endif
