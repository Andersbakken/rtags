#include "NodeType.h"
#include <assert.h>

const char *nodeTypeToName(NodeType type, bool abbrev)
{
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
    assert(0 && "Invalid type");
    return "Invalid";
}
