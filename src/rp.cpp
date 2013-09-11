#include "GccArguments.h"
#include "RTagsClang.h"
#include <clang-c/Index.h>
#include <rct/Log.h>
#include <rct/String.h>

class Scope
{
public:
    Scope(CXIndex &index, CXTranslationUnit &unit)
        : mIndex(index), mUnit(unit)
    {}

    ~Scope()
    {
        if (mIndex) {
            clang_disposeIndex(mIndex);
            mIndex = 0;
        }
        if (mUnit) {
            clang_disposeTranslationUnit(mUnit);
            mUnit = 0;
        }
    }
private:
    CXIndex &mIndex;
    CXTranslationUnit &mUnit;
};

int main(int argc, char **argv)
{
    initLogging();
    List<String> args(argc - 1);
    for (int i=1; i<argc - 1; ++i)
        args[i - 1] = argv[i];

    Path path(argv[argc - 1]);
    CXIndex index = clang_createIndex(0, 1);
    CXTranslationUnit unit = 0;
    Scope scope(index, unit);
    const String contents = path.readAll();
    CXUnsavedFile unsaved = { path.constData(), contents.constData(), static_cast<unsigned long>(contents.size()) };
    String clangLine;
    RTags::parseTranslationUnit(argv[argc - 1], args, List<String>(), unit, index, clangLine, &unsaved, 1);
    if (!unit) {
        fprintf(stderr, "Failed to parse translation unit\n");
        return 1;
    } else {
        char tempFile[PATH_MAX];
        strcpy(tempFile, "/tmp/rtags-tu-XXXXXX");
        const int fd = mkstemp(tempFile);
        if (fd < 0) {
            fprintf(stderr, "Failed to create tempfile\n");
            return 2;
        }
        if (clang_saveTranslationUnit(unit, tempFile, clang_defaultSaveOptions(unit)) == CXSaveError_None) {
            printf("%s\n", tempFile);
            fprintf(stderr, "Wrote to file: %s\n", tempFile);
        } else {
            fprintf(stderr, "Failed to save translation unit\n");
            close(fd);
            return 3;
        }
        close(fd);
    }

    return 0;
}
