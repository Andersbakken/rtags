#include <clang-c/Index.h>

int main(int argc, char **argv)
{
    CXIndex index = clang_createIndex(1, 1);
    const char *args[] = { "-Xclang", "-include-pch=Index.pch" }
    CXTranslationUnit unit = clang_parseTranslationUnit(index, "main.cpp", "-x"



        return 0;
}
