#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <limits.h>
#include <assert.h>
#include "Shared.h"
#include <getopt.h>

static int32_t idLength = -1;
static int caseInsensitive = 0;

static int find(const void *l, const void *r)
{
    const char *left = ((const char*)l) + Int32Length;
    const char *right = ((const char*)r) + Int32Length;
    // printf("%s %s %s\n", left, right, std::string(left, idLength).c_str());
    return (caseInsensitive
            ? strncasecmp(left, right, idLength - Int32Length)
            : strncmp(left, right, idLength - Int32Length));
}


void recurse(const char *ch, int32_t pos, int indent)
{
    struct NodeData node = readNodeData(ch + pos);
    int i;
    for (i=0; i<indent; ++i) {
        printf(" ");
    }
    printf("%s %s %s\n", nodeTypeToName(node.type, Normal), node.symbolName,
           node.location ? ch + node.location : "");
    if (node.firstChild)
        recurse(ch, node.firstChild, indent + 2);
    if (node.nextSibling)
        recurse(ch, node.nextSibling, indent);
}

static inline void usage(const char* argv0, FILE *f)
{
    fprintf(f,
            "%s [options]...\n"
            "  --help|-h                  Display this help\n"
            "  --follow-symbol|-s [arg]   Follow this symbol (e.g. /tmp/main.cpp:32:1)\n"
            "  --references|-r [arg]      Print references of symbol at arg\n"
            "  --print-tree|-t            Print out the node tree to stdout\n"
            "  --list-symbols|-l [arg]    Print out symbols matching arg\n"
            "  --db-file|-f [arg]         Use this database file\n"
            "  --match-complete-symbol|-c Match only complete symbols (for --list-symbols)\n"
            "  --match-starts-with|-S     Match symbols that starts with the search term (for --list-symbols)\n"
            "  --case-insensitive|-i      Case insensitive matching\n"
            "  --no-location|-n           Don't print out the location\n"
            "  --completion-mode|-C       Output usable for completions from say emacs\n",
            argv0);
}

static inline void findReferences(struct NodeData *parent, struct MMapData *mmapData)
{
    if (parent->firstChild) {
        struct NodeData child = readNodeData(mmapData->memory + parent->firstChild);
        while (1) {
            if (child.type == Reference && child.location) {
                printf("%s:%s\n", mmapData->memory + child.location, child.symbolName);
            }
            if (!child.nextSibling)
                break;
            child = readNodeData(mmapData->memory + child.nextSibling);
        }
    }
}

int main(int argc, char **argv)
{
    struct option longOptions[] = {
        { "help", 0, 0, 'h' },
        { "follow-symbol", 1, 0, 's' },
        { "print-tree", 0, 0, 't' },
        { "db-file", 1, 0, 'f' },
        { "references", 1, 0, 'r' },
        { "list-symbols", 1, 0, 'l' },
        { "completion-mode", 0, 0, 'C' },
        { "match-complete-symbol", 0, 0, 'c' },
        { "match-starts-with", 0, 0, 'S' },
        { "case-insensitive", 0, 0, 'i' },
        { 0, 0, 0, 0 },
    };
    const char *shortOptions = "hs:tf:r:l:cSinC";
    int idx, longIndex;
    const char *arg = 0;
    const char *dbFile = 0;
    enum MatchType {
        MatchAnywhere,
        MatchStartsWith,
        MatchCompleteSymbol
    } matchType = MatchAnywhere;
    enum Mode {
        None,
        FollowSymbol,
        References,
        ListSymbols,
        ShowTree
    } mode = None;
    int completionMode = 0;
    struct MMapData mmapData;
    char dbFileBuffer[PATH_MAX + 10];
    while ((idx = getopt_long(argc, argv, shortOptions, longOptions, &longIndex)) != -1) {
        switch (idx) {
        case '?':
            usage(argv[0], stderr);
            return 1;
        case 'i':
            caseInsensitive = 1;
            break;
        case 'h':
            usage(argv[0], stdout);
            return 0;
        case 's':
            if (mode != None) {
                fprintf(stderr, "%s %d: if (mode != None) {\n", __FILE__, __LINE__);
                return 1;
            }
            arg = optarg;
            mode = FollowSymbol;
            break;
        case 'r':
            arg = optarg;
            if (mode != None) {
                fprintf(stderr, "%s %d: if (mode != None) {\n", __FILE__, __LINE__);
                return 1;
            }
            mode = References;
            break;
        case 't':
            if (mode != None) {
                fprintf(stderr, "%s %d: if (mode != None) {\n", __FILE__, __LINE__);
                return 1;
            }
            mode = ShowTree;
            break;
        case 'f':
            dbFile = optarg;
            break;
        case 'l':
            if (mode != None) {
                fprintf(stderr, "%s %d: if (mode != None) {\n", __FILE__, __LINE__);
                return 1;
            }
            mode = ListSymbols;
            arg = optarg;
            break;
        case 'C':
            completionMode = 1;
            break;
        case 'S':
            if (matchType != MatchAnywhere) {
                fprintf(stderr, "%s %d: if (matchType != MatchAnywhere) {\n", __FILE__, __LINE__);
                return 1;
            }
            matchType = MatchStartsWith;
            break;
        case 'c':
            if (matchType != MatchAnywhere) {
                fprintf(stderr, "%s %d: if (matchType != MatchAnywhere) {\n", __FILE__, __LINE__);
                return 1;
            }
            matchType = MatchCompleteSymbol;
            break;
        }
    }
    if (matchType != MatchAnywhere && mode != ListSymbols) {
        fprintf(stderr, "%s %d: if (matchType != MatchAnywhere && mode != ListSymbols)\n", __FILE__, __LINE__);
        return 1;
    }

    if (completionMode && mode != ListSymbols) {
        fprintf(stderr, "%s %d: if (completionMode && mode != ListSymbols) {\n", __FILE__, __LINE__);
        return 1;
    }

    if (!dbFile) {
        if (!findDB(dbFileBuffer, sizeof(dbFileBuffer) - 1)) {
            fprintf(stderr, "%s %d: if (!dbFile) {\n", __FILE__, __LINE__);
            return 1;
        } else {
            dbFile = dbFileBuffer;
        }
    }

    if (mode == None) {
        fprintf(stderr, "%s %d: if (mode == None) {\n", __FILE__, __LINE__);
        return 1;
    }

    if (!loadDb(dbFile, &mmapData)) {
        fprintf(stderr, "%s %d: if (!loadDb(dbFile)) {\n", __FILE__, __LINE__);
        return 1;
    }
    idLength = mmapData.idLength;

    switch (mode) {
    case None:
        assert(0);
        break;
    case ShowTree:
        recurse(mmapData.memory, rootNodePosition(mmapData.nodeCount, idLength), 0);
        break;
    case References:
    case FollowSymbol: {
        assert(arg);
        const int argLen = strlen(arg) + 1;
        char *padded = (char*)malloc(argLen + Int32Length);
        strncpy(padded + Int32Length, arg, argLen);
        const char *bs = (const char*)bsearch(padded, mmapData.memory + FirstId, mmapData.nodeCount, idLength, find);
        // fprintf(stderr, "Found a match %p\n", bs);
        free(padded);
        if (bs) {
            const int32_t idx = readInt32(bs);
            struct NodeData node = readNodeData(mmapData.memory + idx);
            if (mode == References) {
                findReferences(&node, &mmapData);
                if (node.type == MethodDefinition) {
                    struct NodeData parent = readNodeData(mmapData.memory + node.parent);
                    assert(parent.firstChild);
                    struct NodeData declaration = readNodeData(mmapData.memory + parent.firstChild);
                    while (1) {
                        if (declaration.type == MethodDeclaration && !strcmp(node.symbolName, declaration.symbolName)) {
                            findReferences(&declaration, &mmapData);
                            break;
                        }
                        if (!declaration.nextSibling)
                            break;
                        declaration = readNodeData(mmapData.memory + declaration.nextSibling);
                    }
                }
            } else {
                // fprintf(stderr, "Found node %s %s\n", nodeTypeToName(type), symbolName);
                int32_t found = 0;
                NodeType targetType = MethodDeclaration;
                switch (node.type) {
                case MethodDeclaration:
                    targetType = MethodDefinition;
                case MethodDefinition: {
                    struct NodeData parent = readNodeData(mmapData.memory + node.parent);
                    assert(parent.firstChild);
                    struct NodeData sibling = readNodeData(mmapData.memory + parent.firstChild);
                    while (1) {
                        if (sibling.type == (int)targetType && !strcmp(node.symbolName, sibling.symbolName)) {
                            found = sibling.location;
                            break;
                        }
                        if (!sibling.nextSibling)
                            break;
                        sibling = readNodeData(mmapData.memory + sibling.nextSibling);
                    }
                    break; }
                case Reference:
                case EnumValue:
                    found = readNodeData(mmapData.memory + node.parent).location;
                    break;
                default:
                    break;
                }
                if (found) {
                    printf("%s\n", mmapData.memory + found);
                } else {
                    fprintf(stderr, "Couldn't find it\n");
                }
            }
        }
        break; }
    case ListSymbols: {
        int i;
        int32_t pos = mmapData.dictionaryPosition;
        const int argLen = strlen(arg);
        for (i=0; i<mmapData.dictionaryCount; ++i) {
            int32_t symbolName = pos;
            assert(mmapData.memory[pos] > 32); // should be a printable character
            const int len = strlen(mmapData.memory + pos);
            assert(len > 0);
            /* fprintf(stderr, "Found symbol %s %d %d\n", mmapData.memory + pos, len, pos); */
            int matched = 0;
            if (!argLen) {
                matched = 1;
            } else {
                switch (matchType) { // ### case-insensitive
                case MatchAnywhere:
                    if (caseInsensitive
                        ? strcasestr(mmapData.memory + symbolName, arg)
                        : strstr(mmapData.memory + symbolName, arg)) {
                        matched = 1;
                    }
                    break;
                case MatchCompleteSymbol:
                    if ((caseInsensitive
                         ? strcasecmp(mmapData.memory + symbolName, arg)
                         : strcmp(mmapData.memory + symbolName, arg)) == 0) {
                        matched = 1;
                    }
                    break;
                case MatchStartsWith:
                    if ((caseInsensitive
                         ? strncasecmp(mmapData.memory + symbolName, arg, argLen)
                         : strncmp(mmapData.memory + symbolName, arg, argLen)) == 0) {
                        matched = 1;
                    }
                    break;
                }
            }

            pos += len + 1;
            while (1) {
                int32_t nodePos = readInt32(mmapData.memory + pos);
                pos += Int32Length;
                if (!nodePos)
                    break;
                if (matched) {
                    struct NodeData node = readNodeData(mmapData.memory + nodePos);
                    if (!completionMode) {
                        printf("%s:%s\n", mmapData.memory + node.location, node.symbolName);
                    } else if (matched++ == 1) { // we only want the first match in completion mode
                        printf("%s\n", mmapData.memory + symbolName);
                    }
                }
            }
        }
        break; }
    }
    munmap((void*)mmapData.memory, mmapData.mappedSize);
    return 0;
}

