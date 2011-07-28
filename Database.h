#ifndef Database_h
#define Database_h

#include <QtCore>
#include <sys/stat.h>
#include <clang-c/Index.h>
#include "Utils.h"

struct Location {
    Location()
        : line(0), column(0)
    {}
    Location(CXCursor cursor)
        : line(0), column(0)
    {
        CXSourceLocation location = clang_getCursorLocation(cursor);
        CXFile file;
        clang_getInstantiationLocation(location, &file, &line, &column, 0);
        fileName = eatString(clang_getFileName(file));
        if (fileName.isEmpty()) { // This seems to happen
            line = column = 0 ;
        } else {
            resolvePath(fileName);
        }
        Q_ASSERT(fileName.isEmpty() == (line == 0 && column == 0));
    }

    bool exists() const
    {
        return fileExists(fileName);
    }

    QByteArray fileName;
    unsigned line, column;
};

struct Symbol {
    Symbol(const QByteArray &n = QByteArray(),
           const Location &l = Location())
        : symbolName(n), location(l)
    {}

    QByteArray symbolName;
    Location location;
};

static inline QDebug operator<<(QDebug dbg, const Location &loc)
{
    if (!loc.exists()) {
        dbg << "Location(null)";
    } else {
        dbg << QString("Location(%1:%2:%3)").
            arg(QString::fromLocal8Bit(loc.fileName)).arg(loc.line).arg(loc.column);
    }
    return dbg;
}

static inline QDebug operator<<(QDebug dbg, const Symbol &s)
{
    if (!s.location.exists()) {
        dbg << "Symbol(null)";
    } else {
        dbg << "Symbol(";
        dbg.nospace() << s.symbolName << ", " << s.location;
        dbg.maybeSpace();
    }
    return dbg;
}


class SymbolName
{
public:
    // enum MatchType {
    //     MatchExact = 0x0, // only foo(int) matches foo(int)
    //     MatchIgnoreArguments = 0x1, //
    //     MatchIgnore
    //     MatchSymbolName, // foo matches foo(int), as well as Namespace::Class:foo(int)
    //     MatchStartsWith, // fo matches foo(int), as well as Namespace::Class:foo(int)
    //     MatchContains // o matches foo(int), as well as Namespace::Class:foo(int)
    // };

    SymbolName()
        : mArgumentsStart(-1), mHash(-1)
    {}

    SymbolName(const QByteArray &name)
        : mArgumentsStart(-1), mHash(-1)
    {
        operator=(name);
    }

    SymbolName(const SymbolName &other)
    {
        operator=(other);
    }

    inline const QByteArray &symbolName() const
    {
        return mSymbolName;
    }

    inline SymbolName &operator=(const QByteArray &name)
    {
        mSymbolName = name;
        Q_ASSERT(!name.startsWith("::"));
        Q_ASSERT(!name.startsWith(" "));
        Q_ASSERT(!name.startsWith("("));
        // int size = mSymbolName.size();
        // int i = 0;
        // while (i < size) {
        //     if (mSymbolName.at(i) == ' ') {
        //         mSymbolName.remove(i, 1);
        //         --size;
        //     } else {
        //         ++i;
        //     }
        // }

        // ### this should always be a fully qualified symbol name
        // ###. E.g. name = '(::foo' is not valid

        mArgumentsStart = -1;
        mSections.clear();
        int i = 0;
        int size = mSymbolName.size();
        bool lastWasColon = false;
        // ### consider using QMetaObject::normalizedSignature
        while (i < size) {
            switch (mSymbolName.at(i)) {
            case ' ':
                mSymbolName.remove(i, 1);
                lastWasColon = false;
                continue;
            case ':':
                if (mArgumentsStart == -1) {
                    if (lastWasColon) {
                        mSections.append(i - 1);
                        Q_ASSERT(i + 1 >= size || mSections.at(i + 1) != ':'); // we don't want three colons in a row
                    } else {
                        lastWasColon = true;
                    }
                }
                break;
            case '(':
                if (mArgumentsStart == -1)
                    mArgumentsStart = i;
                lastWasColon = false;
                break;
            default:
                lastWasColon = false;
                break;
            }
            ++i;
        }
        mHash = qHash(mSymbolName);
        return *this;
    }

    inline int sectionCount() const
    {
        return mSections.size() + 1 + (mArgumentsStart == -1 ? 0 : 1);
    }

    inline QList<QByteArray> sections() const // slow
    {
        // foo::bar::test(asdasdasdasdasd) => foo bar test (asdasdasdasdas)
        QList<QByteArray> ret;
        int last = 0;
        const int count = mSections.size();
        for (int i=0; i<count; ++i) {
            const int idx = mSections.at(i);
            ret.append(mSymbolName.mid(last, idx - last));
            last = idx + 2;
        }
        if (mArgumentsStart) {
            ret.append(mSymbolName.mid(last, mArgumentsStart - last));
            last = mArgumentsStart;
        }
        if (last < mSymbolName.size())
            ret.append(mSymbolName.mid(last));
        return ret;
    }

    // inline bool match(const QByteArray &symbolName, MatchType type)
    // {
    //     Q_ASSERT(!symbolName.contains(' '));
    //     switch (type) {
    //     case MatchExact:
    //         Q_ASSERT(0 && "this should never be called, use qHash to find things with ExactMatch");
    //         return (symbolName == mSymbolName);
    //     case MatchSymbolName:
    //     case MatchStartsWith:
    //     case MatchContains:
    //         break;
    //     }
    //     return false;
    // }

    inline SymbolName &operator=(const SymbolName &other)
    {
        mArgumentsStart = other.mArgumentsStart;
        mSymbolName = other.mSymbolName;
        mSections = other.mSections;
        mHash = other.mHash;
        return *this;
    }

    inline uint hash() const
    {
        return mHash;
    }
private:
    QByteArray mSymbolName;
    QList<int> mSections; // better datastructure?
    int mArgumentsStart, mHash;
};

static inline uint qHash(const SymbolName &name)
{
    return qHash(name.hash());
}

class Database
{
public:
    static void clear();

    static void addFile(const QByteArray &file, const QList<QByteArray> &compilerOptions);
    static bool removeFile(const QByteArray &file);
    static QList<QByteArray> compilerOptions(const QByteArray &absoluteFilePath);
    static QList<QByteArray> takeCompilerOptions(const QByteArray &absoluteFilePath);

    static void removeReferences(const QByteArray &absoluteFilePath);
    static void setSymbolDeclaration(const QByteArray &symbolName, const Location &location);
    static bool clearSymbolDeclaration(const QByteArray &symbolName);
    static QList<Symbol> lookupDeclarations(const QByteArray &symbolName, bool exactMatch);
    static int symbolDeclarationSize();

    static void setSymbolDefinition(const QByteArray &symbolName, const Location &location);
    static QList<Symbol> lookupDefinitions(const QByteArray &symbolName, bool exactMatch);
    static bool clearSymbolDefinition(const QByteArray &symbolName);
    static int symbolDefinitionSize();

    static void addSymbolReference(const QByteArray &symbolName, const Location &location);
    static QList<Symbol> lookupReferences(const QByteArray &symbolName, bool exactMatch);
    static int clearSymbolReferences(const QByteArray &symbolName);
    static int symbolReferencesSize();
};

#endif
