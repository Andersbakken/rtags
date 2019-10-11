#ifndef ASTCURSOR_H
#define ASTCURSOR_H

#include <memory>
#include <clang-c/Index.h>
#include "RTags.h"
#include <string>
#include "SourceLocation.h"
#include "SourceRange.h"

class AST;
class CursorType;
class Cursor : public std::enable_shared_from_this<Cursor>
{
public:
    Cursor() {}
    std::shared_ptr<SourceLocation> location() const;

    std::string usr() const { return mUsr; }
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
    std::shared_ptr<SourceRange> range() const { return std::make_shared<SourceRange>(clang_getCursorExtent(mCursor)); }
    unsigned overriddenCount() const
    {
        unsigned count = 0;
        CXCursor *overridden = nullptr;
        clang_getOverriddenCursors(mCursor, &overridden, &count);
        if (overridden)
            clang_disposeOverriddenCursors(overridden);
        return count;
    }
    std::vector<std::shared_ptr<Cursor> > overriddenCursors() const;

    unsigned argumentCount() const { return RTags::cursorArguments(mCursor); }
    std::vector<std::shared_ptr<Cursor> > arguments() const;

    int fieldBitWidth() const;
    std::shared_ptr<CursorType> typedefUnderlyingType() const;
    std::shared_ptr<CursorType> enumIntegerType() const;
    long long enumConstantValue() const;
    std::string includedFile() const
    {
        if (clang_getCursorKind(mCursor) == CXCursor_InclusionDirective) {
            CXFile includedFile = clang_getIncludedFile(mCursor);
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
        return clang_Cursor_getNumTemplateArguments(mCursor);
#else
        return 0;
#endif
    }
    std::shared_ptr<CursorType> templateArgumentType(unsigned idx) const;
    long long templateArgumentValue(unsigned idx) const
    {
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 28)
        return clang_Cursor_getTemplateArgumentValue(mCursor, idx);
#else
        (void)idx;
        return -1;
#endif
    }
    std::string templateArgumentKind(unsigned idx) const;

    std::shared_ptr<Cursor> referenced() const;
    std::shared_ptr<Cursor> canonical() const;
    std::shared_ptr<Cursor> lexicalParent() const;
    std::shared_ptr<Cursor> semanticParent() const;
    std::shared_ptr<Cursor> definitionCursor() const;
    std::shared_ptr<Cursor> specializedCursorTemplate() const;
    int childCount() const { return mChildren.size(); }
    std::shared_ptr<Cursor> child(int idx) const { return mChildren.value(idx); }
    std::vector<std::shared_ptr<Cursor> > children() const;
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
        const unsigned result = callback(*this);
        if (result & Add)
            ret.push_back(*this);
        if (result & Recurse && depth > 0) {
            for (const std::shared_ptr<Cursor> &child : mChildren) {
                const auto query = child->query(callback, depth - 1);
                ret.insert(ret.end(), query.begin(), query.end());
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

    CXCursor cxCursor() const { return mCursor; }
    AST *ast() const { return mAst; }

private:
    friend class AST;
    Cursor(AST *a, const std::shared_ptr<Cursor> &p, const CXCursor &c, const std::string &u = std::string())
        : mAst(a), mParent(p), mCursor(c), mUsr(u)
    {
    }

    AST *mAst;
    std::weak_ptr<Cursor> mParent;
    List<std::shared_ptr<Cursor> > mChildren;
    CXCursor mCursor;
    mutable std::shared_ptr<SourceLocation> mLocation;
    std::string mUsr;
};

#endif /* CURSOR_H */
