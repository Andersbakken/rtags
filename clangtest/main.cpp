#include <clang-c/Index.h>
#include <string.h>
#include <stdlib.h>
#include <QtCore>

// static inline const char *kindToString(CXIdxEntityKind kind)
// {
//     switch (kind) {
//     case CXIdxEntity_Unexposed: return "Unexposed";
//     case CXIdxEntity_Typedef: return "Typedef";
//     case CXIdxEntity_Function: return "Function";
//     case CXIdxEntity_Variable: return "Variable";
//     case CXIdxEntity_Field: return "Field";
//     case CXIdxEntity_EnumConstant: return "EnumConstant";
//     case CXIdxEntity_ObjCClass: return "ObjCClass";
//     case CXIdxEntity_ObjCProtocol: return "ObjCProtocol";
//     case CXIdxEntity_ObjCCategory: return "ObjCCategory";
//     case CXIdxEntity_ObjCInstanceMethod: return "ObjCInstanceMethod";
//     case CXIdxEntity_ObjCClassMethod: return "ObjCClassMethod";
//     case CXIdxEntity_ObjCProperty: return "ObjCProperty";
//     case CXIdxEntity_ObjCIvar: return "ObjCIvar";
//     case CXIdxEntity_Enum: return "Enum";
//     case CXIdxEntity_Struct: return "Struct";
//     case CXIdxEntity_Union: return "Union";
//     case CXIdxEntity_CXXClass: return "CXXClass";
//     case CXIdxEntity_CXXNamespace: return "CXXNamespace";
//     case CXIdxEntity_CXXNamespaceAlias: return "CXXNamespaceAlias";
//     case CXIdxEntity_CXXStaticVariable: return "CXXStaticVariable";
//     case CXIdxEntity_CXXStaticMethod: return "CXXStaticMethod";
//     case CXIdxEntity_CXXInstanceMethod: return "CXXInstanceMethod";
//     case CXIdxEntity_CXXConstructor: return "CXXConstructor";
//     case CXIdxEntity_CXXDestructor: return "CXXDestructor";
//     case CXIdxEntity_CXXConversionFunction: return "CXXConversionFunction";
//     case CXIdxEntity_CXXTypeAlias: return "CXXTypeAlias";
//     }
//     return "";
// }

class String
{
    String(const String &other);
    String &operator=(const String &other);
public:
    String(CXString s)
        : str(s)
    {}

    ~String()
    {
        clang_disposeString(str);
    }
    const char *data() const
    {
        return clang_getCString(str);
    }

    CXString str;
};

// static inline CXChildVisitResult visitAll(CXCursor cursor, CXCursor, CXClientData)
// {
//     CXFile f;
//     unsigned l, c;
//     clang_getInstantiationLocation(clang_getCursorLocation(cursor), &f, &l, &c, 0);

//     printf("[%s] [%s] %s:%u:%u:\n",
//            String(clang_getCursorKindSpelling(clang_getCursorKind(cursor))).data(),
//            String(clang_getCursorSpelling(cursor)).data(),
//            String(clang_getFileName(f)).data(),
//            l, c);
//     return CXChildVisit_Recurse;
// }

static inline CXChildVisitResult visitAll(CXCursor cursor, CXCursor, CXClientData)
{
    // switch (clang_getCursorKind(cursor)) {
    // case CXCursor_ParmDecl:
    //     return CXChildVisit_Recurse;
    // case CXCursor_TypeRef:
    // case CXCursor_TemplateRef: {
    CXFile file;
    unsigned line, col;
    clang_getInstantiationLocation(clang_getCursorLocation(cursor), &file, &line, &col, 0);

    CXCursor ref = clang_getCursorReferenced(cursor);
    CXFile file2;
    unsigned line2, col2;
    clang_getInstantiationLocation(clang_getCursorLocation(ref), &file2, &line2, &col2, 0);
    printf("    %s:%u:%u %s %s: references %s:%u:%u %s %s\n",
           String(clang_getFileName(file)).data(),
           line, col,
           String(clang_getCursorKindSpelling(clang_getCursorKind(cursor))).data(),
           String(clang_getCursorSpelling(cursor)).data(),
           String(clang_getFileName(file2)).data(),
           line2, col2,
           String(clang_getCursorKindSpelling(clang_getCursorKind(ref))).data(),
           String(clang_getCursorSpelling(ref)).data());
    return CXChildVisit_Recurse;
}

// void indexDeclaration(CXClientData, const CXIdxDeclInfo *decl)
// {
//     CXFile f;
//     unsigned l, c;
//     clang_indexLoc_getFileLocation(decl->loc, 0, &f, &l, &c, 0);
//     printf("%s:%d:%d: %s %s\n",
//            String(clang_getFileName(f)).data(),
//            l, c, kindToString(decl->entityInfo->kind), decl->entityInfo->name);
//     switch (decl->entityInfo->kind) {
//     case CXIdxEntity_Field:
//     case CXIdxEntity_Variable:
//     case CXIdxEntity_Function:
//     case CXIdxEntity_CXXInstanceMethod:
//     case CXIdxEntity_CXXConstructor:
//         clang_visitChildren(decl->cursor, visitor, 0);
//         break;
//     default:
//         break;
//     }
// }

// void indexEntityReference(CXClientData, const CXIdxEntityRefInfo *ref)
// {
//     CXFile f;
//     unsigned l, c;
//     clang_indexLoc_getFileLocation(ref->loc, 0, &f, &l, &c, 0);

//     CXSourceLocation loc = clang_getCursorLocation(ref->referencedEntity->cursor);
//     CXFile f2;
//     unsigned l2, c2;
//     clang_getInstantiationLocation(loc, &f2, &l2, &c2, 0);
//     printf("%s:%d:%d: ref of %s %s %s:%d:%d\n",
//            String(clang_getFileName(f)).data(), l, c,
//            kindToString(ref->referencedEntity->kind),
//            ref->referencedEntity->name,
//            String(clang_getFileName(f2)).data(), l2, c2);
// }


int main(int argc, char **argv)
{
    QElapsedTimer timer;
    timer.start();
    CXIndex index = clang_createIndex(1, 1);
#if 1
    // const char *argsPch[] = { "-I.", "-x", "c++-header" };
    // CXTranslationUnit unit = clang_parseTranslationUnit(index, "pch.h",
    //                                                     argsPch, sizeof(argsPch) / 4,
    //                                                     0, 0, CXTranslationUnit_Incomplete);
    // // clang_saveTranslationUnit(unit, "/tmp/pch.pch", 0);
    // QByteArray pch;
    // QFile f("/tmp/pch.pch2");
    // f.open(QIODevice::ReadOnly);
    // pch = f.readAll();
    // CXUnsavedFile file = { "/tmp/pch.pch", pch.constData(), pch.size() };
    // clang_disposeTranslationUnit(unit);
    const char *args[] = { "-fsyntax-only", "-I.", "-x", "c++" }; //, "-include-pch", "/tmp/pch.pch" };
    CXTranslationUnit unit = clang_parseTranslationUnit(index, "test.cpp",
                                                        args, sizeof(args) / 4,
                                                        0, 0, clang_defaultEditingTranslationUnitOptions());
    if (unit) {
        clang_visitChildren(clang_getTranslationUnitCursor(unit), visitAll, 0);
        clang_disposeTranslationUnit(unit);
    }

#else
    CXIndexAction action = clang_IndexAction_create(index);
    const char *args[] = { "-cc1", "-I.", "-x", "c++" };
    IndexerCallbacks cb;
    memset(&cb, 0, sizeof(IndexerCallbacks));
    cb.indexDeclaration = indexDeclaration;
    cb.indexEntityReference = indexEntityReference;
    CXTranslationUnit unit = 0;

    const char* filename = (argc < 2 ? "test.cpp" : argv[1]);
    const int completeLine = (argc < 3 ? 15 : atoi(argv[2]));
    const int completeColumn = (argc < 4 ? 7 : atoi(argv[3]));

    clang_indexSourceFile(action, 0, &cb, sizeof(IndexerCallbacks),
                          CXIndexOpt_IndexFunctionLocalSymbols,
                          filename,
                          args, sizeof(args) / 4,
                          0, 0, &unit, clang_defaultEditingTranslationUnitOptions());
    clang_visitChildren(clang_getTranslationUnitCursor(unit), visitAll, 0);

    CXCodeCompleteResults* res = clang_codeCompleteAt(unit, filename,
                                                      completeLine, completeColumn,
                                                      0, 0,
                                                      clang_defaultCodeCompleteOptions());
    if (res) {
        printf("\n---\ncompletions for %s:%d:%d\n", filename, completeLine, completeColumn);
        for (unsigned int i = 0; i < res->NumResults; ++i) {
            const CXCompletionString& str = res->Results[i].CompletionString;
            for (unsigned int j = 0; j < clang_getNumCompletionChunks(str); ++j) {
                if (clang_getCompletionChunkKind(str, j) != CXCompletionChunk_TypedText)
                    continue;

                CXString out = clang_getCompletionChunkText(str, j);
                printf("  %s\n", clang_getCString(out));
                clang_disposeString(out);
            }
        }

        clang_disposeCodeCompleteResults(res);
    }

    if (unit)
        clang_disposeTranslationUnit(unit);
    clang_IndexAction_dispose(action);
#endif
    clang_disposeIndex(index);
    return 0;
}
