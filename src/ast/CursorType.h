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
    std::shared_ptr<Cursor> declaration() const;
    std::string callingConvention() const;
    std::string referenceType() const;
    unsigned argumentCount() const { return clang_getNumArgTypes(type); }
    std::shared_ptr<CursorType> argument(unsigned idx) const { return std::make_shared<CursorType>(ast, clang_getArgType(type, idx)); }
    unsigned templateArgumentCount() const
    {
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 20)
        return clang_Type_getNumTemplateArguments(type);
#else
        return 0;
#endif
    }
    std::shared_ptr<CursorType> templateArgument(unsigned idx) const
    {
#if CINDEX_VERSION > CINDEX_VERSION_ENCODE(0, 20)
        return std::make_shared<CursorType>(ast, clang_Type_getTemplateArgumentAsType(type, idx));
#else
        (void)idx;
        return nullptr;
#endif
    }
    std::shared_ptr<CursorType> canonicalType() const { return std::make_shared<CursorType>(ast, clang_getCanonicalType(type)); }
    std::shared_ptr<CursorType> pointeeType() const { return std::make_shared<CursorType>(ast, clang_getPointeeType(type)); }
    std::shared_ptr<CursorType> resultType() const { return std::make_shared<CursorType>(ast, clang_getResultType(type)); }
    std::shared_ptr<CursorType> elementType() const { return std::make_shared<CursorType>(ast, clang_getElementType(type)); }
    std::shared_ptr<CursorType> arrayElementType() const { return std::make_shared<CursorType>(ast, clang_getElementType(type)); }
    std::shared_ptr<CursorType> classType() const
    {
#if CINDEX_VERSION >= CINDEX_VERSION_ENCODE(0, 20)
        return std::make_shared<CursorType>(ast, clang_Type_getClassType(type));
#else
        return nullptr;
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
