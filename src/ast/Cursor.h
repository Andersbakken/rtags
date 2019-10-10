#ifndef ASTCURSOR_H
#define ASTCURSOR_H

#include <memory>
#include <clang-c/Index.h>
#include "RTags.h"
#include <string>
#include "SourceLocation.h"
#include "SourceRange.h"

class AST;
struct CursorType;
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
    std::string usr() const;
    std::string kind() const;
    std::string linkage() const;
    std::string availability() const;
    std::string language() const;
    std::string spelling() const;
    std::string displayName() const;
    std::string rawComment() const;
    std::string briefComment() const;

    std::string mangledName() const;
    std::string templateKind() const;
    SourceRange range() const { return data ? SourceRange { clang_getCursorExtent(data->cursor) } : SourceRange(); }
    unsigned overriddenCount() const
    {
        unsigned count = 0;
        if (data) {
            CXCursor *overridden = nullptr;
            clang_getOverriddenCursors(data->cursor, &overridden, &count);
            if (overridden)
                clang_disposeOverriddenCursors(overridden);
        }
        return count;
    }
    std::vector<Cursor> overriddenCursors() const;

    unsigned argumentCount() const { return data ? RTags::cursorArguments(data->cursor) : 0; }
    std::vector<Cursor> arguments() const;

    int fieldBitWidth() const;
    CursorType typedefUnderlyingType() const;
    CursorType enumIntegerType() const;
    long long enumConstantValue() const;
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
    CursorType templateArgumentType(unsigned idx) const;
    long long templateArgumentValue(unsigned idx) const
    {
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 28)
        return data ? clang_Cursor_getTemplateArgumentValue(data->cursor, idx) : -1;
#else
        (void)idx;
        return -1;
#endif
    }
    std::string templateArgumentKind(unsigned idx) const;

    Cursor referenced() const;
    Cursor canonical() const;
    Cursor lexicalParent() const;
    Cursor semanticParent() const;
    Cursor definitionCursor() const;
    Cursor specializedCursorTemplate() const;
    int childCount() const { return data ? data->children.size() : 0; }
    Cursor child(int idx) const { return data ? Cursor{ data->children.value(idx)->shared_from_this() } : Cursor(); }
    std::vector<Cursor> children() const;
    enum QueryResult {
        None = 0x0,
        Add = 0x1,
        Recurse = 0x2
    };
    unsigned none() const { return None; };
    unsigned add() const { return Add; };
    unsigned recurse() const { return Recurse; };
    std::vector<Cursor> query(const std::function<unsigned(const Cursor&)> callback, int depth = INT_MAX) const
    {
        std::vector<Cursor> ret;
        if (data) {
            const unsigned result = callback(*this);
            if (result & Add)
                ret.push_back(*this);
            if (result & Recurse && depth > 0) {
                for (Data *childData : data->children) {
                    const Cursor child = { childData->shared_from_this() };
                    const auto query = child.query(callback, depth - 1);
                    ret.insert(ret.end(), query.begin(), query.end());
                }
            }
        }
        return ret;
    }

    bool isBitField() const;
    bool isVirtualBase() const;
    bool isStatic() const;
    bool isVirtual() const;
    bool isPureVirtual() const;
    bool isConst() const;
    bool isDefinition() const;
    bool isDynamicCall() const;

    std::shared_ptr<Data> data;
};

#endif /* CURSOR_H */
