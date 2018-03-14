#ifndef AST_h
#define AST_h

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

#include <clang-c/Index.h>
#include "Source.h"
#include "Location.h"
#include "RTags.h"
#include "rct/String.h"
#include "rct/List.h"

namespace sel {
class State;
};
class AST
{
public:
    template <typename T>
    static std::string toString(T t)
    {
        String str;
        Log(&str) << t;
        return str.ref();
    }
    static std::string toString(CXString str)
    {
        return RTags::eatString(str).ref();
    }
    struct SourceLocation {
        SourceLocation()
            : offset(-1)
        {}

        bool operator<(const SourceLocation &other) const { return location < other.location; }

        bool isNull() const { return location.isNull(); }
        Location location;
        int offset;

        int line() const { return location.line(); }
        int column() const { return location.column(); }
        std::string file() const { return location.path(); }
        std::string toString() const
        {
            return String::format<256>("%s,%d",
                                       location.toString(Location::AbsolutePath|Location::NoColor).constData(),
                                       offset);
        }

    };

    struct SourceRange {
        SourceRange(const CXSourceRange &r = clang_getNullRange())
            : range(r)
        {}
        CXSourceRange range;

        SourceLocation start() const { return AST::createLocation(clang_getRangeStart(range)); }
        SourceLocation end() const { return AST::createLocation(clang_getRangeEnd(range)); }
        int length() const { return end().offset - start().offset; }
        std::string toString() const { return start().toString() + " - " + end().toString(); }
    };
    struct CursorType;
    struct Cursors;
    struct Cursor {
        struct Data : public std::enable_shared_from_this<Data> {
            Data(AST *a, Data *p, const CXCursor &c, const SourceLocation &loc, const std::string &u = std::string())
                : ast(a), parent(p), cursor(c), location(loc), usr(u)
            {
            }
            ~Data()
            {
                for (Data *child : children)
                    delete child;
            }

            AST *ast;
            Data *parent;
            List<Data*> children;
            CXCursor cursor;
            SourceLocation location;
            std::string usr;
        };

        SourceLocation location() const { return data ? data->location : SourceLocation(); }
        std::string usr() const { return data ? data->usr : std::string(); }
        std::string kind() const { return stringProperty(&clang_getCursorKind); }
        std::string linkage() const { return stringProperty(&clang_getCursorLinkage); }
        std::string availability() const { return stringProperty(&clang_getCursorAvailability); }
        std::string language() const { return stringProperty(&clang_getCursorAvailability); }
        std::string spelling() const { return stringProperty(&clang_getCursorSpelling); }
        std::string displayName() const { return stringProperty(&clang_getCursorDisplayName); }
        std::string rawComment() const { return stringProperty(&clang_Cursor_getRawCommentText); }
        std::string briefComment() const { return stringProperty(&clang_Cursor_getBriefCommentText); }

        std::string mangledName() const
        {
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 28)
            return stringProperty(&clang_Cursor_getMangling);
#else
            return std::string();
#endif
        }
        std::string templateKind() const { return stringProperty(&clang_getTemplateCursorKind); }
        SourceRange range() const { return data ? SourceRange { clang_getCursorExtent(data->cursor) } : SourceRange(); }
        unsigned overriddenCount() const
        {
            unsigned count = 0;
            if (data) {
                CXCursor *overridden = 0;
                clang_getOverriddenCursors(data->cursor, &overridden, &count);
                if (overridden)
                    clang_disposeOverriddenCursors(overridden);
            }
            return count;
        }
        Cursors overriddenCursors() const
        {
            Cursors ret;
            if (data) {
                CXCursor *overridden = 0;
                unsigned count;
                clang_getOverriddenCursors(data->cursor, &overridden, &count);
                ret.resize(count);
                for (unsigned i=0; i<count; ++i)
                    ret[i] = data->ast->create(overridden[i]);
                clang_disposeOverriddenCursors(overridden);
            }
            return ret;
        }

        unsigned argumentCount() const { return data ? RTags::cursorArguments(data->cursor) : 0; }
        inline Cursors arguments() const;

        int fieldBitWidth() const { return intProperty(&clang_getFieldDeclBitWidth); }
        CursorType typedefUnderlyingType() const { return cursorTypeProperty(&clang_getTypedefDeclUnderlyingType); }
        CursorType enumIntegerType() const { return cursorTypeProperty(&clang_getEnumDeclIntegerType); }
        long long enumConstantValue() const { return intProperty(&clang_getEnumConstantDeclValue); }
        std::string includedFile() const
        {
            if (data && clang_getCursorKind(data->cursor) == CXCursor_InclusionDirective) {
                CXFile includedFile = clang_getIncludedFile(data->cursor);
                if (includedFile) {
                    CXStringScope fn = clang_getFileName(includedFile);
                    const char *cstr = clang_getCString(fn);
                    if (cstr)
                        return Path::resolved(cstr).ref();
                }
            }
            return std::string();
        }

        unsigned templateArgumentCount() const
        {
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 28)
             return data ? clang_Cursor_getNumTemplateArguments(data->cursor) : 0;
#else
             return 0;
#endif
        }
        CursorType templateArgumentType(unsigned idx) const
        {
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 28)
            return data ? CursorType(data->ast, clang_Cursor_getTemplateArgumentType(data->cursor, idx)) : CursorType();
#else
            (void)idx;
            return CursorType();
#endif
        }
        long long templateArgumentValue(unsigned idx) const
        {
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 28)
            return data ? clang_Cursor_getTemplateArgumentValue(data->cursor, idx) : -1;
#else
            (void)idx;
            return -1;
#endif
        }
        std::string templateArgumentKind(unsigned idx) const
        {
            std::string ret;
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 28)
            if (data)
                ret = toString(clang_Cursor_getTemplateArgumentKind(data->cursor, idx));
#else
            (void)idx;
#endif
            return ret;
        }

        Cursor referenced() const { return cursorProperty(&clang_getCursorReferenced); }
        Cursor canonical() const { return cursorProperty(&clang_getCanonicalCursor); }
        Cursor lexicalParent() const { return cursorProperty(&clang_getCursorLexicalParent); }
        Cursor semanticParent() const { return cursorProperty(&clang_getCursorSemanticParent); }
        Cursor definitionCursor() const { return cursorProperty(& clang_getCursorDefinition); }
        Cursor specializedCursorTemplate() const { return cursorProperty(&clang_getSpecializedCursorTemplate); }
        int childCount() const { return data ? data->children.size() : 0; }
        Cursor child(int idx) const { return data ? Cursor{ data->children.value(idx)->shared_from_this() } : Cursor(); }
        Cursors children() const;
        enum QueryResult {
            None = 0x0,
            Add = 0x1,
            Recurse = 0x2
        };
        unsigned none() const { return None; };
        unsigned add() const { return Add; };
        unsigned recurse() const { return Recurse; };
        Cursors query(const std::function<unsigned(const Cursor&)> callback, int depth = INT_MAX) const
        {
            Cursors ret;
            if (data) {
                const unsigned result = callback(*this);
                if (result & Add)
                    ret.append(*this);
                if (result & Recurse && depth > 0) {
                    for (Data *childData : data->children) {
                        const Cursor child = { childData->shared_from_this() };
                        ret.append(child.query(callback, depth - 1));
                    }
                }
            }
            return ret;
        }

        bool isBitField() const { return intProperty<unsigned, bool>(&clang_Cursor_isBitField); }
        bool isVirtualBase() const { return intProperty<unsigned, bool>(&clang_isVirtualBase); }
        bool isStatic() const { return intProperty<unsigned, bool>(&clang_CXXMethod_isStatic); }
        bool isVirtual() const { return intProperty<unsigned, bool>(&clang_CXXMethod_isVirtual); }
        bool isPureVirtual() const
        {
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 20)
            return intProperty<unsigned, bool>(&clang_CXXMethod_isPureVirtual);
#else
            return false;
#endif
        }
        bool isConst() const
        {
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 20)
            return intProperty<unsigned, bool>(&clang_CXXMethod_isConst);
#else
            return false;
#endif
        }
        bool isDefinition() const { return intProperty<unsigned, bool>(&clang_isCursorDefinition); }
        bool isDynamicCall() const { return intProperty<int, bool>(&clang_Cursor_isDynamicCall); }

        template <typename Func> std::string stringProperty(Func func) const { return data ? toString(func(data->cursor)) : std::string(); }
        template <typename Type, typename Ret = Type> Ret intProperty(Type (*func)(CXCursor), Type defaultValue = Type()) const { return data ? func(data->cursor) : defaultValue; }
        template <typename Func> Cursor cursorProperty(Func func) const { return data ? data->ast->create(func(data->cursor)) : Cursor(); }
        template <typename Func> CursorType cursorTypeProperty(Func func) const { return data ? CursorType(data->ast, func(data->cursor)) : CursorType(); }

        std::shared_ptr<Data> data;
    };

    struct Cursors : public List<Cursor>
    {
        int size() const { return List<Cursor>::size(); }
        Cursor at(int idx) const { return value(idx); }
    };

    struct CursorType {
        CursorType(const AST *a, const CXType &t) : ast(a), type(t) {}
        CursorType(const AST *a = 0) : ast(a) { memset(&type, 0, sizeof(type)); }

        std::string spelling() const { return toString(clang_getTypeSpelling(type)); }
        Cursor declaration() const { return ast ? ast->create(clang_getTypeDeclaration(type)) : Cursor(); }
        std::string callingConvention() const { return toString(clang_getFunctionTypeCallingConv(type)); }
        std::string referenceType() const
        {
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 20)
            return toString(clang_Type_getCXXRefQualifier(type));
#else
            return std::string();
#endif
        }
        unsigned argumentCount() const { return clang_getNumArgTypes(type); }
        CursorType argument(unsigned idx) const { return CursorType(ast, clang_getArgType(type, idx)); }
        unsigned templateArgumentCount() const
        {
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 20)
            return clang_Type_getNumTemplateArguments(type);
#else
            return 0;
#endif
        }
        CursorType templateArgument(unsigned idx) const
        {
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 20)
            return CursorType(ast, clang_Type_getTemplateArgumentAsType(type, idx));
#else
            (void)idx;
            return CursorType();
#endif
        }
        CursorType canonicalType() const { return CursorType(ast, clang_getCanonicalType(type)); }
        CursorType pointeeType() const { return CursorType(ast, clang_getPointeeType(type)); }
        CursorType resultType() const { return CursorType(ast, clang_getResultType(type)); }
        CursorType elementType() const { return CursorType(ast, clang_getElementType(type)); }
        CursorType arrayElementType() const { return CursorType(ast, clang_getElementType(type)); }
        CursorType classType() const
        {
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 20)
            return CursorType(ast, clang_Type_getClassType(type));
#else
            return CursorType();
#endif
        }

        bool isConstQualified() const { return clang_isConstQualifiedType(type); }
        bool isEstrictQualified() const { return clang_isRestrictQualifiedType(type); }
        bool isVolatileQualified() const { return clang_isVolatileQualifiedType(type); }
        bool isVariadic() const { return clang_isFunctionTypeVariadic(type); }
        bool isPod() const { return clang_isPODType(type); }
        long long numElements() const { return clang_getNumElements(type); }
        long long arraySize() const { return clang_getArraySize(type); }
        long long alignOf() const { return clang_Type_getAlignOf(type); }
        long long sizeOf() const { return clang_Type_getSizeOf(type); }

        const AST *ast;
        CXType type;
    };

    struct Diagnostic {


    };

    struct SkippedRange {


    };

    static std::shared_ptr<AST> create(const Source &source, const String &sourceCode, CXTranslationUnit unit);
    List<String> evaluate(const String &script);
    Cursor *root() const { return mRoot; }
    List<Diagnostic> diagnostics() const;
    List<SkippedRange> skippedRanges() const;
    static SourceLocation createLocation(const CXCursor &cursor) { return createLocation(clang_getCursorLocation(cursor)); }
    static SourceLocation createLocation(const CXSourceLocation &location)
    {
        SourceLocation loc;
        loc.location = RTags::createLocation(location, &loc.offset);
        return loc;
    }

    Cursor create(const CXCursor &cursor) const
    {
        if (clang_isInvalid(clang_getCursorKind(cursor)))
            return Cursor();

        auto match = [&cursor](const Cursors &cursors) -> Cursor {
            for (const Cursor &c : cursors) {
                assert(c.data);
                if (clang_equalCursors(c.data->cursor, cursor)) {
                    return c;
                }
            }
            // The explicit const cast is here to satisfy travis clang matrix.
            // clang 3.4 does not allow to use two different return types
            return Cursor();
        };

        const std::string usr = toString(clang_getCursorUSR(cursor));
        if (!usr.empty()) {
            auto it = mByUsr.find(usr);
            if (it != mByUsr.end()) {
                const Cursor ret = match(it->second);
                if (ret.data)
                    return ret;
            }
        }

        const SourceLocation loc = createLocation(cursor);
        if (!loc.isNull()) {
            auto it = mByLocation.find(loc);
            if (it != mByLocation.end()) {
                const Cursor ret = match(it->second);
                if (ret.data)
                    return ret;
            }
        }
        return construct(cursor, 0, loc, usr);
    }
private:
    static CXChildVisitResult visitor(CXCursor cursor, CXCursor, CXClientData u);
    Cursor construct(const CXCursor &cursor,
                     Cursor::Data *parent = 0,
                     SourceLocation loc = SourceLocation(),
                     std::string usr = std::string()) const
    {
        Cursor ret;
        if (loc.isNull())
            loc = createLocation(cursor);
        if (usr.empty())
            usr = toString(clang_getCursorUSR(cursor));
        ret.data.reset(new Cursor::Data(const_cast<AST*>(this), parent, cursor, loc, usr));
        if (!loc.isNull())
            mByLocation[loc].append(ret);
        if (!ret.data->usr.empty())
            mByUsr[usr].append(ret);
        if (parent)
            parent->children.append(ret.data.get());
        return ret;
    }
    AST()
        : mRoot(0)
    {}
    mutable Hash<std::string, Cursors> mByUsr;
    mutable Map<SourceLocation, Cursors> mByLocation;
    String mSourceCode;
    List<String> mReturnValues;
    Cursor *mRoot;
    std::shared_ptr<sel::State> mState;
};


inline AST::Cursors AST::Cursor::children() const
{
    Cursors ret;
    if (data) {
        ret.resize(data->children.size());
        int i = 0;
        for (Data *child : data->children) {
            ret[i++] = Cursor { child->shared_from_this() };
        }
    }
    return ret;
}

inline AST::Cursors AST::Cursor::arguments() const
{
    AST::Cursors ret;
    if (data) {
        List<CXCursor> args;
        const unsigned size = RTags::cursorArguments(data->cursor, &args);
        if (size) {
            ret.resize(size);
            for (unsigned i=0; i<size; ++i) {
                ret[i] = data->ast->create(args[i]);
            }
        }
    }
    return ret;
}
#endif
