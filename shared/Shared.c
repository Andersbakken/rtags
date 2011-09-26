#include "Shared.h"
#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/mman.h>

int findDB(char *dbFileBuffer, int max)
{
    if (getcwd(dbFileBuffer, max)) {
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
                return 1;
            }
            *slash = '\0';
        }
    }
    return 0;
}


const char *nodeTypeToName(int t, NodeTypeToNameMode abbrev)
{
    NodeType type = (NodeType)t;
    switch (type) {
    case Enum: return abbrev ? "e" : "Enum";
    case EnumValue: return abbrev ? "ev" : "EnumValue";
    case Root: return abbrev ? "r" : "Root";
    case MethodDeclaration: return abbrev ? "ml" : "MethodDeclaration";
    case MethodDefinition: return abbrev ? "md" : "MethodDefinition";
    case Class: return abbrev ? "c" : "Class";
    case Struct: return abbrev ? "s" : "Struct";
    case Reference: return abbrev ? "pr" : "Reference";
    case Namespace: return abbrev ? "n" : "Namespace";
    case Typedef: return abbrev ? "t" : "Typedef";
    case Variable: return abbrev ? "vd" : "Variable";
    case MacroDefinition: return abbrev ? "m" : "MacroDefinition";
    case Invalid:
    case All:
        break;
    }
    printf("Invalid node type %d (%d)\n", t, abbrev);
    assert(0 && "Invalid type");
    return "Invalid";
}

NodeType stringToNodeType(const char *in)
{
    if (in) {
        int i;
        for (i=MethodDeclaration; i<=Reference; i <<= 1) {
            const NodeType type = (NodeType)i;
            const char *name = nodeTypeToName(type, Abbreviated);
            assert(name);
            if (!strcasecmp(name, in)) {
                return (NodeType)i;
            }
        }
    }
    return Invalid;
}

int loadDb(const char *dbFile, struct MMapData *data)
{
    assert(data);
    memset(data, 0, sizeof(struct MMapData));
    int fd = 0;
    struct stat st;
    const char *ch = 0;

    fd = open(dbFile, O_RDONLY);
    if (fd <= 0) {
        printf("%s %d: if (fd <= 0)\n", __FILE__, __LINE__);
        return 0;
    }

    if (fstat(fd, &st) < 0) {
        printf("%s %d: if (fstat(fdin, &st) < 0) \n", __FILE__, __LINE__);
        return 0;
    }
    if (st.st_size < HeaderSize) {
        printf("%s %d: if (st.st_size < 10) {\n", __FILE__, __LINE__);
        return 0;
    }
    if ((data->memory = mmap(0, st.st_size, PROT_READ, MAP_SHARED, fd, 0)) == MAP_FAILED) {
        printf("%s %d: if ((src = mmap(0, st.st_size, PROT_READ, MAP_SHARED, fd, 0)) == MAP_FAILED) {\n", __FILE__, __LINE__);
        return 0;
    }
    ch = (char*)data->memory;
    if (strncmp(data->memory, "Rt", 3)) {
        printf("%s %d: if (memcmp(data->memory, \"Rt\", 2)) {\n", __FILE__, __LINE__);
        munmap(data->memory, st.st_size);
        return 0;
    }

    data->nodeCount = readInt32(ch + NodeCountPos);
    data->idLength = readInt32(ch + IdLengthPos);
    data->dictionaryPosition = readInt32(ch + DictionaryPosPos);
    data->dictionaryCount = readInt32(ch + DictionaryCountPos);
    /* printf("locationLength %d nodeCount %d\n" */
    /*        "dictionaryPosition %d dictionaryCount %d\n", locationLength, nodeCount, dictionaryPosition, dictionaryCount); */
    if (data->idLength <= 0 || data->nodeCount <= 0) {
        munmap(data->memory, st.st_size);
        printf("%s %d: if (locationLength <= 0 || nodeCount <= 0)\n", __FILE__, __LINE__);
        return 0;
    }
    data->fileDataPosition = readInt32(ch + FileDataPosPos);
    data->fileDataCount = readInt32(ch + FileDataCountPos);
    data->mappedSize = st.st_size;
    return 1;
}
