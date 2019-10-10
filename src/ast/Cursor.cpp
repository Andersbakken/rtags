#include "AST.h"
#include "Cursor.h"
#include <RTags.h>

template <typename Func> static std::string stringProperty(const std::shared_ptr<Cursor::Data> &data, Func func)
{
    return data ? AST::toString(func(data->cursor)) : std::string();
}

template <typename Type, typename Ret = Type>
static Ret intProperty(const std::shared_ptr<Cursor::Data> &data, Type (*func)(CXCursor), Type defaultValue = Type())
{
    return data ? func(data->cursor) : defaultValue;
}

template <typename Func> static Cursor cursorProperty(const std::shared_ptr<Cursor::Data> &data, Func func)
{
    return data ? data->ast->create(func(data->cursor)) : Cursor();
}

template <typename Func> static CursorType cursorTypeProperty(const std::shared_ptr<Cursor::Data> &data, Func func)
{
    return data ? CursorType(data->ast, func(data->cursor)) : CursorType();
}

std::vector<Cursor> Cursor::children() const
{
    std::vector<Cursor> ret;
    if (data) {
        ret.resize(data->children.size());
        int i = 0;
        for (Data *child : data->children) {
            ret[i++] = Cursor { child->shared_from_this() };
        }
    }
    return ret;
}

std::vector<Cursor> Cursor::arguments() const
{
    std::vector<Cursor> ret;
    if (data) {
        List<CXCursor> args;
        const unsigned size = RTags::cursorArguments(data->cursor, &args);
        if (size) {
            ret.resize(size);
            for (unsigned i = 0; i < size; ++i) {
                ret[i] = data->ast->create(args[i]);
            }
        }
    }
    return ret;
}

std::vector<Cursor> Cursor::overriddenCursors() const
{
    std::vector<Cursor> ret;
    if (data) {
        CXCursor *overridden = nullptr;
        unsigned count;
        clang_getOverriddenCursors(data->cursor, &overridden, &count);
        ret.resize(count);
        for (unsigned i = 0; i < count; ++i)
            ret[i] = data->ast->create(overridden[i]);
        clang_disposeOverriddenCursors(overridden);
    }
    return ret;
}

CursorType Cursor::typedefUnderlyingType() const
{
    return cursorTypeProperty(data, &clang_getTypedefDeclUnderlyingType);
}

CursorType Cursor::enumIntegerType() const
{
    return cursorTypeProperty(data, &clang_getEnumDeclIntegerType);
}

CursorType Cursor::templateArgumentType(unsigned idx) const
{
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 28)
    return data ? CursorType(data->ast, clang_Cursor_getTemplateArgumentType(data->cursor, idx)) : CursorType();
#else
    (void)idx;
    return CursorType();
#endif
}

std::string Cursor::templateArgumentKind(unsigned idx) const
{
    std::string ret;
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 28)
    if (data)
        ret = AST::toString(clang_Cursor_getTemplateArgumentKind(data->cursor, idx));
#else
    (void)idx;
#endif
    return ret;
}

std::string Cursor::kind() const
{
    return stringProperty(data, &clang_getCursorKind);
}
std::string Cursor::linkage() const
{
    return stringProperty(data, &clang_getCursorLinkage);
}
std::string Cursor::availability() const
{
    return stringProperty(data, &clang_getCursorAvailability);
}
std::string Cursor::language() const
{
    return stringProperty(data, &clang_getCursorAvailability);
}
std::string Cursor::spelling() const
{
    return stringProperty(data, &clang_getCursorSpelling);
}
std::string Cursor::displayName() const
{
    return stringProperty(data, &clang_getCursorDisplayName);
}
std::string Cursor::rawComment() const
{
    return stringProperty(data, &clang_Cursor_getRawCommentText);
}
std::string Cursor::briefComment() const
{
    return stringProperty(data, &clang_Cursor_getBriefCommentText);
}

std::string Cursor::mangledName() const
{
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 28)
    return stringProperty(data, &clang_Cursor_getMangling);
#else
    return std::string();
#endif
}

std::string Cursor::templateKind() const
{
    return stringProperty(data, &clang_getTemplateCursorKind);
}

int Cursor::fieldBitWidth() const
{
    return intProperty(data, &clang_getFieldDeclBitWidth);
}

long long Cursor::enumConstantValue() const
{
    return intProperty(data, &clang_getEnumConstantDeclValue);
}

Cursor Cursor::referenced() const
{
    return cursorProperty(data, &clang_getCursorReferenced);
}
Cursor Cursor::canonical() const
{
    return cursorProperty(data, &clang_getCanonicalCursor);
}
Cursor Cursor::lexicalParent() const
{
    return cursorProperty(data, &clang_getCursorLexicalParent);
}
Cursor Cursor::semanticParent() const
{
    return cursorProperty(data, &clang_getCursorSemanticParent);
}
Cursor Cursor::definitionCursor() const
{
    return cursorProperty(data, &clang_getCursorDefinition);
}
Cursor Cursor::specializedCursorTemplate() const
{
    return cursorProperty(data, &clang_getSpecializedCursorTemplate);
}

bool Cursor::isBitField() const
{
    return intProperty<unsigned, bool>(data, &clang_Cursor_isBitField);
}
bool Cursor::isVirtualBase() const
{
    return intProperty<unsigned, bool>(data, &clang_isVirtualBase);
}
bool Cursor::isStatic() const
{
    return intProperty<unsigned, bool>(data, &clang_CXXMethod_isStatic);
}
bool Cursor::isVirtual() const
{
    return intProperty<unsigned, bool>(data, &clang_CXXMethod_isVirtual);
}
bool Cursor::isPureVirtual() const
{
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 20)
    return intProperty<unsigned, bool>(data, &clang_CXXMethod_isPureVirtual);
#else
    return false;
#endif
}
bool Cursor::isConst() const
{
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 20)
    return intProperty<unsigned, bool>(data, &clang_CXXMethod_isConst);
#else
    return false;
#endif
}
bool Cursor::isDefinition() const
{
    return intProperty<unsigned, bool>(data, &clang_isCursorDefinition);
}
bool Cursor::isDynamicCall() const
{
    return intProperty<int, bool>(data, &clang_Cursor_isDynamicCall);
}
