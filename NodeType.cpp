#include "NodeType.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>

const char *nodeTypeToName(int t, NodeTypeToNameMode mode)
{
    const bool abbrev = (mode == Abbreviated);
    NodeType type = static_cast<NodeType>(t);
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
        for (int i=MethodDeclaration; i<=Reference; i <<= 1) {
            const NodeType type = static_cast<NodeType>(i);
            const char *name = nodeTypeToName(type, Abbreviated);
            assert(name);
            if (!strcasecmp(name, in)) {
                return static_cast<NodeType>(i);
            }
        }
    }
    return Invalid;
}
