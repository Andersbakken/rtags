#include <clang-c/Index.h>

void printCursor(CXCursor cursor)
{
    CXSourceLocation loc = clang_getCursorLocation(cursor);
    CXFile file;
    unsigned line, column, offset;
    clang_getInstantiationLocation(loc, &file, &line, &column, &offset);
    CXString fileName = clang_getFileName(file);
    // const char *fileNameStr = clang_getCString(fileName);
    CXString symbolName = clang_getCursorDisplayName(cursor);
    CXString kindName = clang_getCursorKindSpelling(clang_getCursorKind(cursor));
    const bool isDef = clang_isCursorDefinition(cursor);
    printf("%s %s ", clang_getCString(symbolName), clang_getCString(kindName));
    clang_disposeString(kindName);
    clang_disposeString(symbolName);
    if (file) {
        printf("%s:%d:%d", clang_getCString(fileName), line, column);
        clang_disposeString(fileName);
    } else {
        printf("(no file)");
    }
    if (isDef)
        printf("(def)");
}

static CXChildVisitResult dumpTree(CXCursor cursor, CXCursor, CXClientData)
{
    for (CXCursor p=clang_getCursorSemanticParent(cursor); !clang_isInvalid(clang_getCursorKind(cursor)); p = clang_getCursorSemanticParent(p)) {
        printf("  ");
    }
    printCursor(cursor);
    printf("\n");
    return CXChildVisit_Recurse;
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        printf("%s %d: if (argc < 2)\n", __FILE__, __LINE__);
        return 1;
    }
    CXIndex index = clang_createIndex(1, 0);

    CXTranslationUnit unit = clang_parseTranslationUnit(index, argv[argc - 1],
                                                        &argv[1], argc - 2, 0, 0,
                                                        // CXTranslationUnit_NestedMacroExpansions
                                                        CXTranslationUnit_DetailedPreprocessingRecord);
    if (!unit) {
        printf("Can't parse %s ", argv[1]);
        for (int i=1; i<argc - 1; ++i) {
            printf(" \"%s\"", argv[i]);
        }
        printf("\n");
    } else {
        clang_visitChildren(clang_getTranslationUnitCursor(unit), dumpTree, 0);
        fflush(stdout);
        clang_disposeTranslationUnit(unit);
    }
    clang_disposeIndex(index);
    return 0;
}
