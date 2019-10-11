#include "AST.h"
#include "Cursor.h"
#include <RTags.h>

template <typename Func> static std::string stringProperty(const Cursor *cursor, Func func)
{
    return AST::toString(func(cursor->cxCursor()));
}

template <typename Type, typename Ret = Type>
static Ret intProperty(const Cursor *cursor, Type (*func)(CXCursor))
{
    return func(cursor->cxCursor());
}

template <typename Func, typename ...Args> static std::shared_ptr<Cursor> cursorProperty(const Cursor *cursor, Func func, Args &&...args)
{
    return cursor->ast()->create(func(cursor->cxCursor(), std::forward<Args>(args)...));
}

template <typename Func, typename ...Args> static std::shared_ptr<CursorType> cursorTypeProperty(const Cursor *cursor, Func func, Args &&...args)
{
    return std::make_shared<CursorType>(cursor->ast(), func(cursor->cxCursor(), std::forward<Args>(args)...));
}

std::vector<std::shared_ptr<Cursor> > Cursor::children() const
{
    return mChildren;
}

std::vector<std::shared_ptr<Cursor> > Cursor::arguments() const
{
    std::vector<std::shared_ptr<Cursor> > ret;
    List<CXCursor> args;
    const unsigned size = RTags::cursorArguments(mCursor, &args);
    if (size) {
        ret.resize(size);
        for (unsigned i = 0; i < size; ++i) {
            ret[i] = mAst->create(args[i]);
        }
    }
    return ret;
}

std::vector<std::shared_ptr<Cursor> > Cursor::overriddenCursors() const
{
    std::vector<std::shared_ptr<Cursor> > ret;
    CXCursor *overridden = nullptr;
    unsigned count;
    clang_getOverriddenCursors(mCursor, &overridden, &count);
    ret.resize(count);
    for (unsigned i = 0; i < count; ++i)
        ret[i] = mAst->create(overridden[i]);
    clang_disposeOverriddenCursors(overridden);
    return ret;
}

std::shared_ptr<CursorType> Cursor::typedefUnderlyingType() const
{
    return cursorTypeProperty(this, &clang_getTypedefDeclUnderlyingType);
}

std::shared_ptr<CursorType> Cursor::enumIntegerType() const
{
    return cursorTypeProperty(this, &clang_getEnumDeclIntegerType);
}

std::shared_ptr<CursorType> Cursor::templateArgumentType(unsigned idx) const
{
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 28)
    return cursorTypeProperty(this,  clang_Cursor_getTemplateArgumentType, idx);
#else
    return nullptr;
#endif
}

std::string Cursor::templateArgumentKind(unsigned idx) const
{
    std::string ret;
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 28)
    ret = AST::toString(clang_Cursor_getTemplateArgumentKind(mCursor, idx));
#else
    (void)idx;
#endif
    return ret;
}

std::string Cursor::kind() const
{
    return stringProperty(this, &clang_getCursorKind);
}

std::string Cursor::linkage() const
{
    return stringProperty(this, &clang_getCursorLinkage);
}

std::string Cursor::availability() const
{
    return stringProperty(this, &clang_getCursorAvailability);
}

std::string Cursor::language() const
{
    return stringProperty(this, &clang_getCursorAvailability);
}

std::string Cursor::spelling() const
{
    return stringProperty(this, &clang_getCursorSpelling);
}

std::string Cursor::displayName() const
{
    return stringProperty(this, &clang_getCursorDisplayName);
}

std::string Cursor::rawComment() const
{
    return stringProperty(this, &clang_Cursor_getRawCommentText);
}

std::string Cursor::briefComment() const
{
    return stringProperty(this, &clang_Cursor_getBriefCommentText);
}

std::string Cursor::mangledName() const
{
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 28)
    return stringProperty(this, &clang_Cursor_getMangling);
#else
    return std::string();
#endif
}

std::string Cursor::templateKind() const
{
    return stringProperty(this, &clang_getTemplateCursorKind);
}

int Cursor::fieldBitWidth() const
{
    return intProperty(this, &clang_getFieldDeclBitWidth);
}

long long Cursor::enumConstantValue() const
{
    return intProperty(this, &clang_getEnumConstantDeclValue);
}

std::shared_ptr<Cursor> Cursor::referenced() const
{
    return cursorProperty(this, &clang_getCursorReferenced);
}

std::shared_ptr<Cursor> Cursor::canonical() const
{
    return cursorProperty(this, &clang_getCanonicalCursor);
}

std::shared_ptr<Cursor> Cursor::lexicalParent() const
{
    return cursorProperty(this, &clang_getCursorLexicalParent);
}

std::shared_ptr<Cursor> Cursor::semanticParent() const
{
    return cursorProperty(this, &clang_getCursorSemanticParent);
}

std::shared_ptr<Cursor> Cursor::definitionCursor() const
{
    return cursorProperty(this, &clang_getCursorDefinition);
}

std::shared_ptr<Cursor> Cursor::specializedCursorTemplate() const
{
    return cursorProperty(this, &clang_getSpecializedCursorTemplate);
}

bool Cursor::isBitField() const
{
    return intProperty<unsigned, bool>(this, &clang_Cursor_isBitField);
}
bool Cursor::isVirtualBase() const
{
    return intProperty<unsigned, bool>(this, &clang_isVirtualBase);
}
bool Cursor::isStatic() const
{
    return intProperty<unsigned, bool>(this, &clang_CXXMethod_isStatic);
}
bool Cursor::isVirtual() const
{
    return intProperty<unsigned, bool>(this, &clang_CXXMethod_isVirtual);
}
bool Cursor::isPureVirtual() const
{
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 20)
    return intProperty<unsigned, bool>(this, &clang_CXXMethod_isPureVirtual);
#else
    return false;
#endif
}
bool Cursor::isConst() const
{
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 20)
    return intProperty<unsigned, bool>(this, &clang_CXXMethod_isConst);
#else
    return false;
#endif
}
bool Cursor::isDefinition() const
{
    return intProperty<unsigned, bool>(this, &clang_isCursorDefinition);
}
bool Cursor::isDynamicCall() const
{
    return intProperty<int, bool>(this, &clang_Cursor_isDynamicCall);
}
