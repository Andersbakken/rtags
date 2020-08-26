// cc clangtest.c `llvm-config-3.8 --cflags` -lclang `llvm-config-3.8 --ldflags` -o clangtest
#include <clang-c/Index.h>
#include <stdio.h>
#include <string.h>

#include "clang-c/CXString.h"

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
    unsigned int off, line, col;
    CXSourceLocation location = clang_getCursorLocation(cursor);
    clang_getSpellingLocation(location, &file, &line, &col, &off);
    CXString fileName = clang_getFileName(file);
    const char *fileNameCStr = clang_getCString(fileName);
    if (fileNameCStr) {
        CXSourceRange range = clang_getCursorExtent(cursor);
        unsigned int start, end;
        clang_getSpellingLocation(clang_getRangeStart(range), nullptr, nullptr, nullptr, &start);
        clang_getSpellingLocation(clang_getRangeEnd(range), nullptr, nullptr, nullptr, &end);
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
    CXTranslationUnit unit = nullptr;
    const char * const *args = nullptr;
    const char *saved = "/tmp/unit";
    if (argc > 2) {
        if (!strcmp("--load", argv[2])) {
            int error = clang_createTranslationUnit2(index, saved, &unit);
            printf("GOT ERROR %d -> %p\n", error, unit);
            if (unit) {
                error = clang_reparseTranslationUnit(unit, 0, nullptr, clang_defaultReparseOptions(unit));
                printf("GOT REPARSE RROR %d -> %p\n", error, unit);
                if (error) {
                    clang_disposeTranslationUnit(unit);
                    unit = nullptr;
                }
            }
            --argc;
        } else {
            args = (const char *const *)&argv[2];
        }
    }

    if (!unit) {
        unit = clang_parseTranslationUnit(index, nullptr, args, argc - 2,
                                          nullptr, 0,
                                          clang_defaultEditingTranslationUnitOptions() | CXTranslationUnit_DetailedPreprocessingRecord | CXTranslationUnit_ForSerialization );
        // FILE *f = fopen(argv[1], "a");
        // fprintf(f, " "); //namespace { int shitty() { return 0; } }\n");
        // fclose(f);
        // int error = clang_reparseTranslationUnit(unit, 0, 0, clang_defaultReparseOptions(unit));
        // printf("GOT REPARSE RROR %d -> %p\n", error, unit);
        // if (error) {
        //     clang_disposeTranslationUnit(unit);
        //     unit = 0;
        // }
    }
    if (unit) {
        int indent = 0;
        clang_visitChildren(clang_getTranslationUnitCursor(unit), visit, &indent);

        const unsigned int diagnosticCount = clang_getNumDiagnostics(unit);
        unsigned int i;
        for (i=0; i<diagnosticCount; ++i) {
            CXDiagnostic diagnostic = clang_getDiagnostic(unit, i);
            const unsigned int diagnosticOptions = (CXDiagnostic_DisplaySourceLocation|
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
        clang_saveTranslationUnit(unit, saved, clang_defaultSaveOptions(unit));
        clang_disposeTranslationUnit(unit);
    }
    clang_disposeIndex(index);
    return 0;
}
