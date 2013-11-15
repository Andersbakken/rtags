#include <clang-c/Index.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

static void printString(const char *name, CXString string)
{
    const char *cstr = clang_getCString(string);
    if (cstr && *cstr) {
        printf("%s: %s ", name, cstr);
    }
    clang_disposeString(string);
}

static void printCursor(CXCursor cursor)
{
    CXFile file;
    unsigned off, line, col;
    CXSourceLocation location = clang_getCursorLocation(cursor);
    clang_getSpellingLocation(location, &file, &line, &col, &off);
    CXString fileName = clang_getFileName(file);
    const char *fileNameCStr = clang_getCString(fileName);
    if (fileNameCStr) {
        CXSourceRange range = clang_getCursorExtent(cursor);
        unsigned start, end;
        clang_getSpellingLocation(clang_getRangeStart(range), 0, 0, 0, &start);
        clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &end);
        printf("%s:%d:%d (%d, %d-%d) ", fileNameCStr, line, col, off, start, end);
    }
    clang_disposeString(fileName);
    printString("kind", clang_getCursorKindSpelling(clang_getCursorKind(cursor)));
    printString("display name", clang_getCursorDisplayName(cursor));
    printString("usr", clang_getCursorUSR(cursor));
    if (clang_isCursorDefinition(cursor))
        printf("definition ");
    printf("\n");
}

static enum CXChildVisitResult visit(CXCursor cursor, CXCursor parent, CXClientData userData)
{
    (void)parent;
    int indent = *(int*)userData;
    int i;
    for (i=0; i<indent; ++i) {
        printf("  ");
    }
    printCursor(cursor);
    CXCursor ref = clang_getCursorReferenced(cursor);
    if (!clang_isInvalid(clang_getCursorKind(ref)) && !clang_equalCursors(ref, cursor)) {
        for (i=0; i<indent; ++i) {
            printf("  ");
        }
        printf("-> ");
        printCursor(ref);
    }
    ++indent;
    clang_visitChildren(cursor, visit, &indent);
    return CXChildVisit_Continue;
}

int main(int argc, char **argv)
{
    if (argc < 2)
        return 1;
    CXIndex index = clang_createIndex(1, 1);
    const char * const *args = 0;
    if (argc > 2)
        args = (const char *const *)&argv[2];

    CXTranslationUnit unit = clang_parseTranslationUnit(index, argv[1], args, argc - 2,
                                                        0, 0, clang_defaultEditingTranslationUnitOptions());
    if (unit) {
        int indent = 0;
        clang_visitChildren(clang_getTranslationUnitCursor(unit), visit, &indent);

        const unsigned diagnosticCount = clang_getNumDiagnostics(unit);
        unsigned i;
        for (i=0; i<diagnosticCount; ++i) {
            CXDiagnostic diagnostic = clang_getDiagnostic(unit, i);
            const unsigned diagnosticOptions = (CXDiagnostic_DisplaySourceLocation|
                                                CXDiagnostic_DisplayColumn|
                                                CXDiagnostic_DisplaySourceRanges|
                                                CXDiagnostic_DisplayOption|
                                                CXDiagnostic_DisplayCategoryId|
                                                CXDiagnostic_DisplayCategoryName);
            CXString diagnosticText = clang_formatDiagnostic(diagnostic, diagnosticOptions);
            const char *cstr = clang_getCString(diagnosticText);
            if (cstr)
                printf("%s\n", cstr);
            clang_disposeString(diagnosticText);
        }
        clang_disposeTranslationUnit(unit);
    }
    clang_disposeIndex(index);
    return 0;
}
