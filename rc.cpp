#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <limits.h>
#include <assert.h>
#include "Shared.h"
#include <getopt.h>
#include <string>

static int32_t locationLength = -1;

enum { FirstId = 2 + sizeof(int32_t) * 3 };

static int find(const void *l, const void *r)
{
    const char *left = reinterpret_cast<const char*>(l) + sizeof(int32_t);
    const char *right = reinterpret_cast<const char*>(r) + sizeof(int32_t);
    // printf("%s %s %s\n", left, right, std::string(left, locationLength).c_str());
    return strncmp(reinterpret_cast<const char*>(left),
                   reinterpret_cast<const char*>(right),
                   locationLength);
}

static inline void readNode(const char *base, int32_t *type, int32_t *location, int32_t *parent,
                            int32_t *nextSibling, int32_t *firstChild, const char **symbolName)
{
    if (type)
        memcpy(type, base, sizeof(int32_t));
    if (location)
        memcpy(location, base + sizeof(int32_t), sizeof(int32_t));
    if (parent)
        memcpy(parent, base + sizeof(int32_t) + sizeof(int32_t), sizeof(int32_t));
    if (nextSibling)
        memcpy(nextSibling, base + sizeof(int32_t) + (sizeof(int32_t) * 2), sizeof(int32_t));
    if (firstChild)
        memcpy(firstChild, base + sizeof(int32_t) + (sizeof(int32_t) * 3), sizeof(int32_t));
    if (symbolName)
        *symbolName = reinterpret_cast<const char*>(base + sizeof(int32_t) + (sizeof(int32_t) * 4));
}

struct Cleanup {
    ~Cleanup()
    {
        if (*mapped)
            munmap(*mapped, st->st_size);
        if (*fd)
            close(*fd);
    }

    int *fd;
    struct stat *st;
    void **mapped;
};

void recurse(const char *ch, int32_t pos, int indent)
{
    int32_t type, location, nextSibling, firstChild;
    const char *symbolName;
    readNode(ch + pos, &type, &location, 0, &nextSibling, &firstChild, &symbolName);
    for (int i=0; i<indent; ++i) {
        printf(" ");
    }
    printf("%s %s %s\n", nodeTypeToName(type), symbolName, location ? ch + location : "");
    if (firstChild)
        recurse(ch, firstChild, indent + 2);
    if (nextSibling)
        recurse(ch, nextSibling, indent);
}

static inline void usage(FILE *f)
{
    fprintf(f,
            "rc [options]...\n"
            "  --help|-h                Display this help\n"
            "  --follow-symbol|-s [arg] Follow this symbol (e.g. /tmp/main.cpp:32:1)\n"
            "  --references|-r [arg]    Print references of symbol at arg\n"
            "  --print-tree|-t          Print out the node tree to stdout\n"
            "  --db-file|-f [arg]       Use this database file\n");
}

int main(int argc, char **argv)
{
    struct option longOptions[] = {
        { "help", 0, 0, 'h' },
        { "follow-symbol", 1, 0, 's' },
        { "print-tree", 0, 0, 't' },
        { "db-file", 1, 0, 'f' },
        { "references", 1, 0, 'r' },
        { 0, 0, 0, 0 },
    };
    const char *shortOptions = "hs:tf:r:";
    int idx;
    int longIndex;
    const char *arg = 0;
    const char *dbFile = 0;
    enum Mode {
        None,
        FollowSymbol,
        References
    } mode = None;
    char dbFileBuffer[PATH_MAX + 10];
    bool showTree = false;
    // for (int i=0; i<argc; ++i) {
    //     printf("%d %s\n", i, argv[i]);
    // }

    while ((idx = getopt_long(argc, argv, shortOptions, longOptions, &longIndex)) != -1) {
        switch (idx) {
        case '?':
            usage(stderr);
            return 1;
        case 'h':
            usage(stdout);
            return 0;
        case 's':
            arg = optarg;
            mode = FollowSymbol;
            break;
        case 'r':
            arg = optarg;
            mode = References;
            break;
        case 't':
            showTree = true;
            break;
        case 'f':
            dbFile = optarg;
            break;
        }
    }

    if (!dbFile) {
        if (getcwd(dbFileBuffer, PATH_MAX)) {
            const int len = strlen(dbFileBuffer);
            if (len > 0 && dbFileBuffer[len - 1] != '/') {
                dbFileBuffer[len] = '/';
                dbFileBuffer[len + 1] = '\0';
            }
            char *slash;
            while ((slash = strrchr(dbFileBuffer, '/'))) {
                // ### this is awful
                strcpy(slash + 1, ".rtags.db");
                struct stat s;
                // printf("Testing [%s]\n", dbFileBuffer);
                if (stat(dbFileBuffer, &s) >= 0) {
                    dbFile = dbFileBuffer;
                    break;
                }
                *slash = '\0';
            }
        }
        if (!dbFile) {
            printf("%s %d: if (!dbFile) {\n", __FILE__, __LINE__);
            return 1;
        }
    }

    if (mode == None && !showTree) {
        printf("%s %d: if (mode == None && !showTree) {\n", __FILE__, __LINE__);
        return 1;
    }
        

    int fd = 0;
    struct stat st;
    void *mapped = 0;
    const char *ch = 0;
    // Cleanup cleanup = { .fd = &fd, .st = &st, .mapped = &mapped };
    Cleanup cleanup = { &fd, &st, &mapped };
    (void)cleanup;
    
    fd = open(dbFile, O_RDONLY);
    if (fd <= 0) {
        printf("%s %d: if (fd <= 0)\n", __FILE__, __LINE__);
        return 1;
    }

    if (fstat(fd, &st) < 0) {
        printf("%s %d: if (fstat(fdin, &st) < 0) \n", __FILE__, __LINE__);
        return 1;
    }
    if (st.st_size < 10) {
        printf("%s %d: if (st.st_size < 10) {\n", __FILE__, __LINE__);
        return 1;
    }
        
    if ((mapped = mmap(0, st.st_size, PROT_READ, MAP_SHARED, fd, 0)) == MAP_FAILED) {
        printf("%s %d: if ((src = mmap(0, st.st_size, PROT_READ, MAP_SHARED, fd, 0)) == MAP_FAILED) {\n", __FILE__, __LINE__);
        return 1;
    }
    ch = reinterpret_cast<char*>(mapped);
    if (memcmp(mapped, "Rt", 2)) {
        printf("%s %d: if (memcmp(mapped, \"Rt\", 2)) {\n", __FILE__, __LINE__);
        return 1;
    }

    // for (int i=0; i<10; ++i) {
    //     printf("%d %x %c\n", i, ch[i], ch[i]);
    // }

    int32_t nodeCount = -1;
    int32_t dictionaryPosition = -1;
    memcpy(&nodeCount, ch + 2, sizeof(int32_t));
    memcpy(&locationLength, ch + 2 + sizeof(int32_t), sizeof(int32_t));
    memcpy(&dictionaryPosition, ch + 2 + sizeof(int32_t) + sizeof(int32_t), sizeof(int32_t));
    printf("%d %d %d\n", locationLength, nodeCount, dictionaryPosition);
    // qDebug() << (locationLength + 1 + sizeof(int32_t));
    if (locationLength <= 0 || nodeCount <= 0) {
        printf("%s %d: if (locationLength <= 0 || nodeCount <= 0)\n", __FILE__, __LINE__);
        return 1;
    }

    if (showTree) {
        recurse(ch, FirstId + ((locationLength + 1 + sizeof(int32_t)) * nodeCount), 0);
    }

    if (arg) {
        const int argLen = strlen(arg) + 1;
        char *padded = new char[argLen + sizeof(int32_t)];
        strncpy(padded + sizeof(int32_t), arg, argLen);
        const void *bs = bsearch(padded,
                                 ch + FirstId,
                                 nodeCount,
                                 locationLength + 1 + sizeof(int32_t),
                                 find);
        // printf("Found a match %p\n", bs);
        delete []padded;
        if (bs) {
            int32_t idx = *reinterpret_cast<const int32_t*>(bs);
            const char *symbolName;
            int32_t type;
            int32_t parent;
            int32_t nextSibling;
            int32_t location;
            readNode(ch + idx, &type, &location, &parent, 0, &nextSibling, &symbolName);
            if (mode == References) {
                while (nextSibling) {
                    if (type == Reference && location) {
                        printf("%s\n", ch + location);
                    }
                    readNode(ch + nextSibling, &type, &location, 0, &nextSibling, 0, 0);
                }
            } else {
                // printf("Found node %s %s\n", nodeTypeToName(type), symbolName);
                int32_t found = 0;
                NodeType targetType = MethodDeclaration;
                switch (type) {
                case MethodDeclaration:
                    targetType = MethodDefinition;
                case MethodDefinition: {
                    const char *f;
                    int32_t firstChild;
                    readNode(ch + parent, &type, 0, 0, 0, &firstChild, &f);
                    assert(firstChild);
                    int32_t next = firstChild;
                    do {
                        const char *symbol;
                        readNode(ch + next, &type, &location, 0, &nextSibling, 0, &symbol);
                        // printf("%s %s %s\n", symbolName, symbol, nodeTypeToName(type));
                        if (type == targetType && !strcmp(symbolName, symbol)) {
                            found = location;
                            break;
                        }
                        next = nextSibling;
                    } while (next);
                    break; }
                case Reference:
                case EnumValue:
                    readNode(ch + parent, 0, &found, 0, 0, 0, &symbolName);
                    break;
                default:
                    break;
                }
                if (found) {
                    printf("%s\n", ch + found);
                } else {
                    printf("Couldn't find it\n");
                }
            }
        }
    }
    return 0;
}

