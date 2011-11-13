#include <clang-c/Index.h>

class String
{
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

int main(int argc, char **argv)
{
    CXIndex index = clang_createIndex(1, 1);
    // const char *args[] = { "-cc1", "-include-pch", "foo.h.pch", "-I.", "-x", "c++" };
    const char *args[] = { "-cc1", "-I.", "-x", "c++" };
    CXTranslationUnit unit = clang_parseTranslationUnit(index, "test.cpp", args, sizeof(args) / 4,
                                                        0, 0, clang_defaultEditingTranslationUnitOptions());

    if (unit) {
        clang_visitChildren(clang_getTranslationUnitCursor(unit), visitor, 0);
    } else {
        printf("fucked\n");
    }
    clang_disposeIndex(index);
    return 0;
}
