#include <clang-c/Index.h>

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

static CXChildVisitResult visitor(CXCursor cursor, CXCursor, CXClientData)
{
    CXFile file;
    unsigned line, col;
    clang_getInstantiationLocation(clang_getCursorLocation(cursor), &file, &line, &col, 0);
    printf("%s %s %s:%u:%u\n",
           String(clang_getCursorKindSpelling(clang_getCursorKind(cursor))).data(),
           String(clang_getCursorSpelling(cursor)).data(),
           String(clang_getFileName(file)).data(),
           line, col);
    return CXChildVisit_Recurse;
}


/**
 * \brief Called when a diagnostic is emitted.
 */
static void diagnostic(CXClientData client_data, CXDiagnostic diagnostic, void *reserved)
{
    printf("%s:%d void diagnostic(CXClientData client_data, CXDiagnostic diagnostic, void *reserved)\n", __FILE__, __LINE__);
}

/**
 * \brief Called for the purpose of associating a client's CXIdxFile with
 * a CXFile.
 */
static CXIdxFile recordFile(CXClientData client_data, CXFile file, void *reserved)
{
    printf("%s:%d CXIdxFile recordFile(CXClientData client_data, CXFile file, void *reserved)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called when a file gets #included/#imported.
 */
static void ppIncludedFile(CXClientData client_data, CXIdxIncludedFileInfo *)
{
    printf("%s:%d void ppIncludedFile(CXClientData client_data, CXIdxIncludedFileInfo *)\n", __FILE__, __LINE__);
}

/**
 * \brief Called when a macro gets #defined.
 */
static CXIdxMacro ppMacroDefined(CXClientData client_data, CXIdxMacroDefinedInfo *)
{
    printf("%s:%d CXIdxMacro ppMacroDefined(CXClientData client_data, CXIdxMacroDefinedInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called when a macro gets undefined.
 */
static void ppMacroUndefined(CXClientData client_data, CXIdxMacroUndefinedInfo *)
{
    printf("%s:%d void ppMacroUndefined(CXClientData client_data, CXIdxMacroUndefinedInfo *)\n", __FILE__, __LINE__);
}

/**
 * \brief Called when a macro gets expanded.
 */
static void ppMacroExpanded(CXClientData client_data, CXIdxMacroExpandedInfo *)
{
    printf("%s:%d void ppMacroExpanded(CXClientData client_data, CXIdxMacroExpandedInfo *)\n", __FILE__, __LINE__);
}
  
/**
 * \brief Called when a AST file (PCH or module) gets imported.
 * 
 * AST files will not get indexed (there will not be callbacks to index all
 * the entities in an AST file). The recommended action is that, if the AST
 * file is not already indexed, to block further indexing and initiate a new
 * indexing job specific to the AST file, so that references of entities of
 * the AST file can be later associated with CXIdxEntities returned by
 * \see importedEntity callbacks.
 */
static CXIdxASTFile importedASTFile(CXClientData client_data, CXIdxImportedASTFileInfo *)
{
    printf("%s:%d CXIdxASTFile importedASTFile(CXClientData client_data, CXIdxImportedASTFileInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called when an entity gets imported from an AST file. This generally
 * happens when an entity from a PCH/module is referenced for the first time.
 */
static CXIdxEntity importedEntity(CXClientData client_data, CXIdxImportedEntityInfo *)
{
    printf("%s:%d CXIdxEntity importedEntity(CXClientData client_data, CXIdxImportedEntityInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called when a macro gets imported from an AST file. This generally
 * happens when a macro from a PCH/module is referenced for the first time.
 */
static CXIdxEntity importedMacro(CXClientData client_data, CXIdxImportedMacroInfo *)
{
    printf("%s:%d CXIdxEntity importedMacro(CXClientData client_data, CXIdxImportedMacroInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called at the beginning of indexing a translation unit.
 */
static CXIdxContainer startedTranslationUnit(CXClientData client_data, void *reserved)
{
    printf("%s:%d CXIdxContainer startedTranslationUnit(CXClientData client_data, void *reserved)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called to index a typedef entity.
 */
static CXIdxEntity indexTypedef(CXClientData client_data, CXIdxTypedefInfo *)
{
    printf("%s:%d CXIdxEntity indexTypedef(CXClientData client_data, CXIdxTypedefInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called to index a function entity.
 */
static CXIdxEntity indexFunction(CXClientData client_data, CXIdxFunctionInfo *)
{
    printf("%s:%d CXIdxEntity indexFunction(CXClientData client_data, CXIdxFunctionInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called to index a function redeclaration.
 */
static void indexFunctionRedeclaration(CXClientData client_data, CXIdxFunctionRedeclInfo *)
{
    printf("%s:%d void indexFunctionRedeclaration(CXClientData client_data, CXIdxFunctionRedeclInfo *)\n", __FILE__, __LINE__);
}

/**
 * \brief Called to index a file-scope variable (not field or ivar).
 */
static CXIdxEntity indexVariable(CXClientData client_data, CXIdxVariableInfo *)
{
    printf("%s:%d CXIdxEntity indexVariable(CXClientData client_data, CXIdxVariableInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called to index a variable redeclaration.
 */
static void indexVariableRedeclaration(CXClientData client_data, CXIdxVariableRedeclInfo *)
{
    printf("%s:%d void indexVariableRedeclaration(CXClientData client_data, CXIdxVariableRedeclInfo *)\n", __FILE__, __LINE__);
}

/**
 * \brief Called to index a tag entity (struct/union/enum/class).
 */
static CXIdxEntity indexTagType(CXClientData client_data, CXIdxTagTypeInfo *)
{
    printf("%s:%d CXIdxEntity indexTagType(CXClientData client_data, CXIdxTagTypeInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called to index a tag redeclaration.
 */
static void indexTagTypeRedeclaration(CXClientData client_data, CXIdxTagTypeRedeclInfo *)
{
    printf("%s:%d void indexTagTypeRedeclaration(CXClientData client_data, CXIdxTagTypeRedeclInfo *)\n", __FILE__, __LINE__);
}

/**
 * \brief Called to index a tag type's field entity.
 */
static CXIdxEntity indexField(CXClientData client_data, CXIdxFieldInfo *)
{
    printf("%s:%d CXIdxEntity indexField(CXClientData client_data, CXIdxFieldInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called to index an enumerator entity.
 */
static CXIdxEntity indexEnumerator(CXClientData client_data, CXIdxEnumeratorInfo *)
{
    printf("%s:%d CXIdxEntity indexEnumerator(CXClientData client_data, CXIdxEnumeratorInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called to initiate a tag type's container context.
 */
static CXIdxContainer startedTagTypeDefinition(CXClientData client_data, CXIdxTagTypeDefinitionInfo *)
{
    printf("%s:%d CXIdxContainer startedTagTypeDefinition(CXClientData client_data, CXIdxTagTypeDefinitionInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called to index an ObjC class entity.
 */
static CXIdxEntity indexObjCClass(CXClientData client_data, CXIdxObjCClassInfo *)
{
    printf("%s:%d CXIdxEntity indexObjCClass(CXClientData client_data, CXIdxObjCClassInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called to index an ObjC protocol entity.
 */
static CXIdxEntity indexObjCProtocol(CXClientData client_data, CXIdxObjCProtocolInfo *)
{
    printf("%s:%d CXIdxEntity indexObjCProtocol(CXClientData client_data, CXIdxObjCProtocolInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called to index an ObjC category entity.
 */
static CXIdxEntity indexObjCCategory(CXClientData client_data, CXIdxObjCCategoryInfo *)
{
    printf("%s:%d CXIdxEntity indexObjCCategory(CXClientData client_data, CXIdxObjCCategoryInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called to index an ObjC method entity.
 */
static CXIdxEntity indexObjCMethod(CXClientData client_data, CXIdxObjCMethodInfo *)
{
    printf("%s:%d CXIdxEntity indexObjCMethod(CXClientData client_data, CXIdxObjCMethodInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called to index an ObjC property entity.
 */
static CXIdxEntity indexObjCProperty(CXClientData client_data, CXIdxObjCPropertyInfo *)
{
    printf("%s:%d CXIdxEntity indexObjCProperty(CXClientData client_data, CXIdxObjCPropertyInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called to index an ObjC method redeclaration.
 */
static void indexObjCMethodRedeclaration(CXClientData client_data, CXIdxObjCMethodRedeclInfo *)
{
    printf("%s:%d void indexObjCMethodRedeclaration(CXClientData client_data, CXIdxObjCMethodRedeclInfo *)\n", __FILE__, __LINE__);
}

/**
 * \brief Called to initiate a statement body container context for a
 * function/ObjC method/C++ member function/block.
 */
static CXIdxContainer startedStatementBody(CXClientData client_data, CXIdxStmtBodyInfo *)
{
    printf("%s:%d CXIdxContainer startedStatementBody(CXClientData client_data, CXIdxStmtBodyInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called to initiate an ObjC container context for
 * @interface/@implementation/@protocol.
 */
static CXIdxContainer startedObjCContainer(CXClientData client_data, CXIdxObjCContainerInfo *)
{
    printf("%s:%d CXIdxContainer startedObjCContainer(CXClientData client_data, CXIdxObjCContainerInfo *)\n", __FILE__, __LINE__);
    return 0;
}

/**
 * \brief Called to define an ObjC class via its @interface.
 */
static void defineObjCClass(CXClientData client_data, CXIdxObjCClassDefineInfo *)
{
    printf("%s:%d void defineObjCClass(CXClientData client_data, CXIdxObjCClassDefineInfo *)\n", __FILE__, __LINE__);
}

/**
 * \brief Called when a container context is ended.
 */
static void endedContainer(CXClientData client_data, CXIdxEndContainerInfo *)
{
    printf("%s:%d void endedContainer(CXClientData client_data, CXIdxEndContainerInfo *)\n", __FILE__, __LINE__);
}

/**
 * \brief Called to index a reference of an entity.
 */
static void indexEntityReference(CXClientData client_data, CXIdxEntityRefInfo *)
{
    printf("%s:%d void indexEntityReference(CXClientData client_data, CXIdxEntityRefInfo *)\n", __FILE__, __LINE__);
}

int main(int argc, char **argv)
{
    CXIndex index = clang_createIndex(1, 1);



    // int ret = clang_indexTranslationUnit(index, 0, 0,
    // const char *args[] = { "-cc1", "-include-pch", "foo.h.pch", "-I.", "-x", "c++" };
    // const char *args[] = { "-cc1", "-I.", "-x", "c++" };
    // CXTranslationUnit unit = clang_parseTranslationUnit(index, "test.cpp", args, sizeof(args) / 4,
    //                                                     0, 0, clang_defaultEditingTranslationUnitOptions());

    // if (unit) {
    //     clang_visitChildren(clang_getTranslationUnitCursor(unit), visitor, 0);
    // } else {
    //     printf("fucked\n");
    // }
    clang_disposeIndex(index);
    return 0;
}
