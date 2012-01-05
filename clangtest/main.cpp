#include <clang-c/Index.h>
#include <string.h>

const char *kindToString(CXIdxEntityKind kind)
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

// static CXChildVisitResult visitor(CXCursor cursor, CXCursor, CXClientData)
// {
//     CXFile file;
//     unsigned line, col;
//     clang_getInstantiationLocation(clang_getCursorLocation(cursor), &file, &line, &col, 0);
//     printf("%s %s %s:%u:%u\n",
//            String(clang_getCursorKindSpelling(clang_getCursorKind(cursor))).data(),
//            String(clang_getCursorSpelling(cursor)).data(),
//            String(clang_getFileName(file)).data(),
//            line, col);
//     return CXChildVisit_Recurse;
// }

/**
 * \brief Called periodically to check whether indexing should be aborted.
 * Should return 0 to continue, and non-zero to abort.
 */
// int abortQuery(CXClientData client_data, void *reserved)
// {
// }

/**
 * \brief Called at the end of indexing; passes the complete diagnostic set.
 */
void diagnostic(CXClientData, CXDiagnosticSet set, void *)
{
    for (unsigned i=0; i<clang_getNumDiagnosticsInSet(set); ++i) {
        CXDiagnostic diagnostic = clang_getDiagnosticInSet(set, i);
        if (clang_getDiagnosticSeverity(diagnostic) >= CXDiagnostic_Warning) {
            printf("Diagnostic: %d %s\n", clang_getDiagnosticSeverity(diagnostic),
                   String(clang_getDiagnosticSpelling(diagnostic)).data());
        }
        clang_disposeDiagnostic(diagnostic);
    }
}

// CXIdxClientFile enteredMainFile(CXClientData client_data,
//                                 CXFile mainFile, void *reserved)
// {
//     printf("enteredMainFile %s\n", String(clang_getFileName(mainFile)).constData());
// }
  
/**
 * \brief Called when a file gets #included/#imported.
 */
// CXIdxClientFile ppIncludedFile(CXClientData client_data,
//                                const CXIdxIncludedFileInfo *info)
// {
// }
  
/**
 * \brief Called when a AST file (PCH or module) gets imported.
 * 
 * AST files will not get indexed (there will not be callbacks to index all
 * the entities in an AST file). The recommended action is that, if the AST
 * file is not already indexed, to block further indexing and initiate a new
 * indexing job specific to the AST file.
 */
// CXIdxClientASTFile importedASTFile(CXClientData client_data,
//                                    const CXIdxImportedASTFileInfo *)
// {
// }

/**
 * \brief Called at the beginning of indexing a translation unit.
 */
// CXIdxClientContainer startedTranslationUnit(CXClientData client_data,
//                                             void *reserved)
// {
// }

static inline void debugCursor(FILE* out, const CXCursor& cursor)
{
    CXFile file;
    unsigned int line, col, off;
    CXSourceLocation loc = clang_getCursorLocation(cursor);
    clang_getInstantiationLocation(loc, &file, &line, &col, &off);
    CXString name = clang_getCursorDisplayName(cursor);
    CXString filename = clang_getFileName(file);
    CXString kind = clang_getCursorKindSpelling(clang_getCursorKind(cursor));
    fprintf(out, "cursor name %s, kind %s, loc %s:%u:%u\n",
            clang_getCString(name), clang_getCString(kind),
            clang_getCString(filename), line, col);
    clang_disposeString(name);
    clang_disposeString(kind);
    clang_disposeString(filename);
}

void indexDeclaration(CXClientData, const CXIdxDeclInfo *decl)
{
    if (decl->isImplicit)
        return;
    CXFile f;
    unsigned l, c;
    clang_indexLoc_getFileLocation(decl->loc, 0, &f, &l, &c, 0);
    printf("%s:%d:%d: %s %s def: %d redecl: %d cont: %d (%s) templateKind %d\n", String(clang_getFileName(f)).data(),
           l, c, decl->entityInfo->name, kindToString(decl->entityInfo->kind),
           decl->isDefinition, decl->isRedeclaration, decl->isContainer, decl->entityInfo->USR,
           decl->entityInfo->templateKind);
    // debugCursor(stdout, decl->cursor);
    // if (decl->isContainer)
    //     debugCursor(stdout, decl->container->cursor);
    // debugCursor(stdout, decl->entityInfo->cursor);
}

/**
 * \brief Called to index a reference of an entity.
 */
void indexEntityReference(CXClientData, const CXIdxEntityRefInfo *ref)
{
    CXFile f;
    unsigned l, c;
    clang_indexLoc_getFileLocation(ref->loc, 0, &f, &l, &c, 0);

    CXSourceLocation loc = clang_getCursorLocation(ref->referencedEntity->cursor);
    CXFile f2;
    unsigned l2, c2;
    clang_getInstantiationLocation(loc, &f2, &l2, &c2, 0);
    printf("%s:%d:%d: ref of %s (%s) %s:%d:%d templatekind %d cursor kind %d/%d\n", String(clang_getFileName(f)).data(),
           l, c, ref->referencedEntity->name, ref->referencedEntity->USR,
           String(clang_getFileName(f2)).data(), l2, c2, ref->referencedEntity->templateKind,
           clang_getCursorKind(ref->cursor), clang_getCursorKind(ref->referencedEntity->cursor));
}


int main(int, char **)
{
    CXIndex index = clang_createIndex(1, 1);
    CXIndexAction action = clang_IndexAction_create(index);

    // int ret = clang_indexTranslationUnit(index, 0, 0,
    // const char *args[] = { "-cc1", "-include-pch", "foo.h.pch", "-I.", "-x", "c++" };
    const char *args[] = { "-cc1", "-I.", "-x", "c++" };
    IndexerCallbacks cb;
    memset(&cb, 0, sizeof(IndexerCallbacks));
    cb.indexDeclaration = indexDeclaration;
    cb.indexEntityReference = indexEntityReference;
    CXTranslationUnit unit = 0;
    int ret = clang_indexSourceFile(action, 0, &cb, sizeof(IndexerCallbacks),
                                    CXIndexOpt_None,
                                    "test.cpp", args, sizeof(args) / 4,
                                    0, 0, &unit, clang_defaultEditingTranslationUnitOptions());
    printf("%d %p\n", ret, unit);

    // CXTranslationUnit unit = clang_parseTranslationUnit(index, "test.cpp", args, sizeof(args) / 4,
    //                                                     0, 0, clang_defaultEditingTranslationUnitOptions());

    if (unit) {
        // clang_visitChildren(clang_getTranslationUnitCursor(unit), visitor, 0);
    } else {
        printf("fucked\n");
    }
    clang_disposeTranslationUnit(unit);
    clang_IndexAction_dispose(action);
    clang_disposeIndex(index);
    return 0;
}
