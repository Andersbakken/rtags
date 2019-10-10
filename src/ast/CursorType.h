#ifndef CURSORTYPE_H
#define CURSORTYPE_H

#include "Cursor.h"
#include <clang-c/Index.h>
#include <string>

class AST;
struct CursorType {
    CursorType(const AST *a, const CXType &t) : ast(a), type(t) {}
    CursorType(const AST *a = nullptr) : ast(a) { memset(&type, 0, sizeof(type)); }

    std::string spelling() const;
    Cursor declaration() const;
    std::string callingConvention() const;
    std::string referenceType() const;
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

#endif /* CURSORTYPE_H */
