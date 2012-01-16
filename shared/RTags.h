#ifndef RTags_h
#define RTags_h

#include <string>
#include <stdlib.h>
#include <QtCore>
#include <leveldb/db.h>
#include <leveldb/write_batch.h>
#include "Path.h"
#include <clang-c/Index.h>

namespace RTags {

bool parseLocation(const std::string &string,
                   std::string &file, unsigned &line, unsigned &col);
Path findRtagsDb(const char *path = 0);
class LevelDBScope
{
public:
    LevelDBScope(leveldb::DB *d)
        : db(d)
    {}
    ~LevelDBScope()
    {
        delete db;
    }
private:
    leveldb::DB *db;
};

static inline const char *kindToString(CXIdxEntityKind kind)
{
    switch (kind) {
    case CXIdxEntity_Unexposed: return "Unexposed";
    case CXIdxEntity_Typedef: return "Typedef";
    case CXIdxEntity_Function: return "Function";
    case CXIdxEntity_Variable: return "Variable";
    case CXIdxEntity_Field: return "Field";
    case CXIdxEntity_EnumConstant: return "EnumConstant";
    case CXIdxEntity_ObjCClass: return "ObjCClass";
    case CXIdxEntity_ObjCProtocol: return "ObjCProtocol";
    case CXIdxEntity_ObjCCategory: return "ObjCCategory";
    case CXIdxEntity_ObjCInstanceMethod: return "ObjCInstanceMethod";
    case CXIdxEntity_ObjCClassMethod: return "ObjCClassMethod";
    case CXIdxEntity_ObjCProperty: return "ObjCProperty";
    case CXIdxEntity_ObjCIvar: return "ObjCIvar";
    case CXIdxEntity_Enum: return "Enum";
    case CXIdxEntity_Struct: return "Struct";
    case CXIdxEntity_Union: return "Union";
    case CXIdxEntity_CXXClass: return "CXXClass";
    case CXIdxEntity_CXXNamespace: return "CXXNamespace";
    case CXIdxEntity_CXXNamespaceAlias: return "CXXNamespaceAlias";
    case CXIdxEntity_CXXStaticVariable: return "CXXStaticVariable";
    case CXIdxEntity_CXXStaticMethod: return "CXXStaticMethod";
    case CXIdxEntity_CXXInstanceMethod: return "CXXInstanceMethod";
    case CXIdxEntity_CXXConstructor: return "CXXConstructor";
    case CXIdxEntity_CXXDestructor: return "CXXDestructor";
    case CXIdxEntity_CXXConversionFunction: return "CXXConversionFunction";
    case CXIdxEntity_CXXTypeAlias: return "CXXTypeAlias";
    }
    return "";
}

QByteArray kindToString(CXCursorKind kind);
const char *completionChunkKindToString(int kind);
bool locationFromString(const QByteArray &string, Path *path = 0, int *line = 0, int *column = 0);
static inline QByteArray eatString(CXString string)
{
    const QByteArray ret = clang_getCString(string);
    clang_disposeString(string);
    return ret;
}

static inline bool isValidCursor(CXCursor cursor)
{
    CXCursorKind kind = clang_getCursorKind(cursor);
    if (!clang_isInvalid(kind)) {
        CXSourceLocation loc = clang_getCursorLocation(cursor);
        CXFile file;
        unsigned int line, col, off;
        clang_getInstantiationLocation(loc, &file, &line, &col, &off);
        CXString filename = clang_getFileName(file);
        const char* str = clang_getCString(filename);
        if (!str || !strcmp(str, "")) {
            clang_disposeString(filename);
            return false;
        }
        clang_disposeString(filename);
        return true;
    }
    return false;
}


static inline void removeWhitespace(QByteArray &ba)
{
    int size = ba.size();
    int i = 0;
    while (i < size) {
        if (ba.at(i) == ' ') {
            ba.remove(i, 1);
            --size;
        } else {
            ++i;
        }
    }
}
static inline std::string removePath(const std::string& line)
{
    const std::string::size_type slash = line.rfind('/');
    if (slash == std::string::npos)
        return line;
    return line.substr(slash + 1);
}

static inline QByteArray removePath(const QByteArray& line)
{
    const int slash = line.lastIndexOf('/');
    if (slash == -1)
        return line;
    return line.mid(slash + 1);
}

static inline bool isValidKind(CXCursorKind kind)
{
    switch (kind) {
    case CXCursor_InvalidFile:
    case CXCursor_NoDeclFound:
    case CXCursor_InitListExpr:
    case CXCursor_StmtExpr:
    case CXCursor_UnexposedDecl:
    case CXCursor_UsingDirective:
    case CXCursor_CXXAccessSpecifier:
    case CXCursor_IntegerLiteral:
    case CXCursor_FloatingLiteral:
    case CXCursor_StringLiteral:
    case CXCursor_CharacterLiteral:
    case CXCursor_ParenExpr:
    case CXCursor_UnaryOperator:
    case CXCursor_ArraySubscriptExpr:
    case CXCursor_BinaryOperator:
    case CXCursor_CompoundAssignOperator:
    case CXCursor_ConditionalOperator:
    case CXCursor_CStyleCastExpr:
    case CXCursor_GNUNullExpr:
    case CXCursor_CXXStaticCastExpr:
    case CXCursor_CXXDynamicCastExpr:
    case CXCursor_CXXReinterpretCastExpr:
    case CXCursor_CXXConstCastExpr:
    case CXCursor_CXXFunctionalCastExpr:
    case CXCursor_CXXBoolLiteralExpr:
    case CXCursor_CXXThisExpr:
    case CXCursor_CXXThrowExpr:
    case CXCursor_CXXNewExpr:
    case CXCursor_CXXDeleteExpr:
    case CXCursor_CompoundStmt:
    case CXCursor_IfStmt:
    case CXCursor_WhileStmt:
    case CXCursor_DoStmt:
    case CXCursor_ForStmt:
    case CXCursor_GotoStmt:
    case CXCursor_ContinueStmt:
    case CXCursor_BreakStmt:
    case CXCursor_ReturnStmt:
    case CXCursor_AsmStmt:
    case CXCursor_CXXCatchStmt:
    case CXCursor_CXXTryStmt:
    case CXCursor_NullStmt:
    case CXCursor_DeclStmt:
    case CXCursor_UnexposedAttr:
        return false;
    default:
        break;
    }
    return true;
}

QDebug operator<<(QDebug dbg, CXCursor cursor);
QDebug operator<<(QDebug dbg, const std::string &str);
QDebug operator<<(QDebug dbg, const leveldb::Slice &slice);
QList<QByteArray> &systemIncludes();
}

#endif
