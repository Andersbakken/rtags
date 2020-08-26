#include "TranslationUnit.h"

#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <string.h>
#include <clang-c/CXCompilationDatabase.h>
#include <stdlib.h>
#include <string>
#include <unordered_set>

#include "Node.h"
#include "clang-c/CXString.h"
#include "clang-c/Index.h"

std::unique_ptr<TranslationUnit> TranslationUnit::create(char **argv, int argc, unsigned int flags)
{
    // for (int i=2; i<argc; ++i) {
    //     printf("[%s] ", argv[i]);
    // }

    const char *const* args = nullptr;
    int argsCount = 0;
    char **allocated = nullptr;
    // should support compile_commands.json

    if (argc == 3) {
        size_t len = strlen(argv[1]);
        if (len >= 21 && !strcmp(argv[1] + len - 21, "compile_commands.json")) {
            std::string dir(argv[1], len - 21);
            CXCompilationDatabase_Error error = CXCompilationDatabase_NoError;
            CXCompilationDatabase dataBase = clang_CompilationDatabase_fromDirectory(dir.c_str(), &error);
            if (!dataBase) {
                fprintf(stderr, "Failed to load compilation database\n");
                return nullptr;
            }

            CXCompileCommands commands = clang_CompilationDatabase_getAllCompileCommands(dataBase);
            assert(commands);
            const unsigned count = clang_CompileCommands_getSize(commands);
            CXCompileCommand matched = nullptr;
            for (unsigned i=0; i<count; ++i) {
                CXCompileCommand command = clang_CompileCommands_getCommand(commands, i);
                assert(command);
                CXString fn = clang_CompileCommand_getFilename(command);
                const char *cstr = clang_getCString(fn);
                if (!strcmp(cstr, argv[2])) {
                    matched = command;
                    clang_disposeString(fn);
                    break;
                } else if (strstr(cstr, argv[2])) {
                    if (!matched) {
                        matched = command;
                    } else {
                        CXString fn2 = clang_CompileCommand_getFilename(matched);
                        const char *cstr2 = clang_getCString(fn2);
                        clang_disposeString(fn2);

                        fprintf(stderr, "Multiple matches for %s\n%s\n%s\n",
                                argv[2], cstr2, cstr);
                        break;
                    }
                }
                clang_disposeString(fn);
            }
            if (matched) {
                argsCount = clang_CompileCommand_getNumArgs(matched);
                allocated = new char*[argsCount + 1];
                allocated[argsCount] = nullptr;
                for (int i=0; i<argsCount; ++i) {
                    CXString arg = clang_CompileCommand_getArg(matched, i);
                    const char *cstr = clang_getCString(arg);
                    allocated[i] = strdup(cstr);
                    clang_disposeString(arg);
                }
                args = const_cast<const char *const*>(allocated);
            }

            clang_CompileCommands_dispose(commands);
            clang_CompilationDatabase_dispose(dataBase);
            if (!matched) {
                fprintf(stderr, "Couldn't find anything matching: \"%s\" in %s",
                        argv[2], argv[1]);
                return nullptr;
            }
        }
    }

    if (!args) {
        int offset = 1;
        if (access(argv[1], X_OK)) {
            ++offset;
        }
        args = const_cast<const char *const *>(&argv[offset]);
        argsCount = argc - offset;
    }

    CXIndex index = clang_createIndex(0, 1);
    CXTranslationUnit unit { nullptr };

    unit = clang_parseTranslationUnit(index,
                                      nullptr,
                                      args,
                                      argsCount,
                                      nullptr,
                                      0,
                                      clang_defaultEditingTranslationUnitOptions() | CXTranslationUnit_DetailedPreprocessingRecord);
    if (allocated) {
        for (int i=0; i<argsCount; ++i) {
            free(allocated[i]);
        }
        delete[] allocated;
    }
    // printf("%p %s\n", unit, source_filename);
    if (!unit) {
        clang_disposeIndex(index);
        return nullptr;
    }

    std::unique_ptr<TranslationUnit> ret(new TranslationUnit);
    ret->mFlags = flags;
    ret->mIndex = index;
    ret->mUnit = unit;
    ret->mRoot = new Node(clang_getTranslationUnitCursor(ret->mUnit), flags);

    return ret;
}

TranslationUnit::~TranslationUnit()
{
    assert(mIndex);
    clang_disposeIndex(mIndex);
    assert(mUnit);
    clang_disposeTranslationUnit(mUnit);
    std::unordered_set<Node *> seen;
    Node::deleteRecursive(mRoot, seen);
}

unsigned int TranslationUnit::flags() const
{
    return mFlags;
}

void TranslationUnit::setFlags(unsigned int flags)
{
    if (flags == mFlags)
        return;
    mFlags = flags;
    std::unordered_set<Node *> seen;
    Node::deleteRecursive(mRoot, seen);
    mRoot = new Node(clang_getTranslationUnitCursor(mUnit), flags);
}
