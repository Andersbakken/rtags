#include "Shared.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

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
