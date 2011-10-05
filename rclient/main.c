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
#include "RClient.h"
#include <getopt.h>

void recurse(const char *ch, int32_t pos, int indent)
{
    struct NodeData node = readNodeData(ch + pos);
    int i;
    for (i=0; i<indent; ++i) {
        printf(" ");
    }
    printf("%s %s %s\n", nodeTypeToName(node.type, Normal), node.symbolName,
           node.location ? ch + node.location : "");
    if (node.type != Reference && node.firstChild) // reuse firstChild as containingFunction for references
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

static inline void findReferences(struct NodeData *parent, struct MMapData *mmapData,
                                  const char **seen, int maxRecursionDepth)
{
    static int idx = 0;
    int indent = 0;
    if (seen) {
        while (seen[indent] && indent < maxRecursionDepth) {
            if (parent->address == seen[indent])
                return;
            ++indent;
        }
        if (indent == maxRecursionDepth)
            return;
        seen[indent] = parent->address;
    }
    if (parent->firstChild) {
        struct NodeData child = readNodeData(mmapData->memory + parent->firstChild);
        while (1) {
            if (child.type == Reference && child.location) {
                int i;
                const char *name;
                struct NodeData containingFunction;
                if (child.containingFunction) {
                    containingFunction = readNodeData(mmapData->memory + child.containingFunction);
                    name = containingFunction.symbolName;
                } else {
                    name = child.symbolName;
                }
                for (i=0; i<indent; ++i)
                    printf(" ");
                if (indent) {
                    printf("%s:%s\n", mmapData->memory + child.location, name);
                } else {
                    printf("%s:%s %d\n", mmapData->memory + child.location, name, ++idx);
                }
                if (seen && child.containingFunction) {
                    findReferences(&containingFunction, mmapData, seen, maxRecursionDepth);
                }
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
        { "recursive-references", 1, 0, 'R' },
        { "max-recursion-reference-depth", 1, 0, 'x' },
        { "list-symbols", 1, 0, 'l' },
        { "completion-mode", 0, 0, 'C' },
        { "match-complete-symbol", 0, 0, 'm' },
        { "match-starts-with", 0, 0, 'S' },
        { "case-insensitive", 0, 0, 'i' },
        { "case-insensitive", 0, 0, 'i' },
        { "config", 1, 0, 'c' },
        { 0, 0, 0, 0 },
    };
    const char *shortOptions = "hs:tf:r:R:l:mSinCx:c:";
    int idx, longIndex;
    const char *arg = 0;
    const char *dbFile = 0;
    int maxRecursionDepth = -1;
    enum MatchType {
        MatchAnywhere,
        MatchStartsWith,
        MatchCompleteSymbol
    } matchType = MatchAnywhere;
    enum Mode {
        None,
        FollowSymbol,
        References,
        RecursiveReferences,
        ListSymbols,
        ShowTree
    } mode = None;
    int completionMode = 0;
    struct MMapData mmapData;
    char dbFileBuffer[PATH_MAX + 10];
    unsigned flags = 0;
    while ((idx = getopt_long(argc, argv, shortOptions, longOptions, &longIndex)) != -1) {
        switch (idx) {
        case '?':
            usage(argv[0], stderr);
            return 1;
        case 'c': {
            int i = 0;
            char buf[1024];
            while (1) {
                if (loadConfiguration(optarg, "foo", buf, 1024, i)) {
                    printf("Got value: [%s]\n", buf);
                    ++i;
                } else {
                    break;
                }
            }
            return 0; }
        case 'i':
            flags |= CaseInsensitive;
            break;
        case 'h':
            usage(argv[0], stdout);
            return 0;
        case 'x':
            maxRecursionDepth = atoi(optarg);
            if (maxRecursionDepth <= 0) {
                fprintf(stderr, "%s %d: if (!maxRecursionDepth) {\n", __FILE__, __LINE__);
                return 1;
            }
            break;
        case 's':
            if (mode != None) {
                fprintf(stderr, "%s %d: if (mode != None) {\n", __FILE__, __LINE__);
                return 1;
            }
            arg = optarg;
            mode = FollowSymbol;
            break;
        case 'r':
        case 'R':
            arg = optarg;
            if (mode != None) {
                fprintf(stderr, "%s %d: if (mode != None) {\n", __FILE__, __LINE__);
                return 1;
            }
            mode = (idx == 'r' ? References : RecursiveReferences);
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
        case 'm':
            if (matchType != MatchAnywhere) {
                fprintf(stderr, "%s %d: if (matchType != MatchAnywhere) {\n", __FILE__, __LINE__);
                return 1;
            }
            matchType = MatchCompleteSymbol;
            break;
        }
    }
    if (mode != RecursiveReferences && maxRecursionDepth != -1) {
        fprintf(stderr, "%s %d: if (mode != RecursiveReferences && maxRecursionDepth != -1) {\n", __FILE__, __LINE__);
        return 1;
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
    initRClient(&mmapData, flags);

    switch (mode) {
    case None:
        assert(0);
        break;
    case ShowTree:
        recurse(mmapData.memory, rootNodePosition(mmapData.nodeCount, mmapData.idLength), 0);
        break;
    case FollowSymbol: {
        struct NodeData node;
        if (findByLocation(arg, &node)) {
            struct NodeData target;
            memset(&target, 0, sizeof(struct NodeData));
            switch (node.type) {
            case MethodDeclaration: 
                findSibling(&node, &target, MethodDefinition);
                break;
            case MethodDefinition:
                findSibling(&node, &target, MethodDeclaration);
                break;
            case Reference:
            case EnumValue:
                target = readNode(node.parent);
                break;
            default:
                break;
            }
            if (target.address) {
                printf("%s\n", mmapData.memory + target.location);
            } else {
                fprintf(stderr, "Couldn't find it\n");
            }
        }
        break; }
    case RecursiveReferences:
    case References: {
        struct NodeData node;
        if (findByLocation(arg, &node)) {
            const char **seen = 0;
            if (mode == RecursiveReferences) {
                if (maxRecursionDepth == -1)
                    maxRecursionDepth = 10;
                seen = malloc(sizeof(const char*) * maxRecursionDepth);
                memset(seen, 0, sizeof(const char *) * maxRecursionDepth);
            }
            if (node.type == Reference)
                node = readNodeData(mmapData.memory + node.parent);
            findReferences(&node, &mmapData, seen, maxRecursionDepth);
            if (node.type == MethodDefinition || node.type == MethodDeclaration) { // find declaration
                const int targetType = (node.type == MethodDeclaration
                                        ? MethodDefinition
                                        : MethodDeclaration);
                struct NodeData sibling;
                if (findSibling(&node, &sibling, targetType))
                    findReferences(&sibling, &mmapData, seen, maxRecursionDepth);
            }
            if (seen)
                free(seen);
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
                    if (flags & CaseInsensitive
                        ? strcasestr(mmapData.memory + symbolName, arg)
                        : strstr(mmapData.memory + symbolName, arg)) {
                        matched = 1;
                    }
                    break;
                case MatchCompleteSymbol:
                    if ((flags & CaseInsensitive
                         ? strcasecmp(mmapData.memory + symbolName, arg)
                         : strcmp(mmapData.memory + symbolName, arg)) == 0) {
                        matched = 1;
                    }
                    break;
                case MatchStartsWith:
                    if ((flags & CaseInsensitive
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

