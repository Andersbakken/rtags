#ifndef NodeType_h
#define NodeType_h

enum NodeType {
    Invalid = 0x000000,
    Root = 0x000001,
    Namespace = 0x000002,
    Class = 0x000004,
    Struct = 0x000008,
    MethodDefinition = 0x000010,
    MethodDeclaration = 0x000020,
    Variable = 0x000040,
    Enum = 0x000080,
    EnumValue = 0x000100,
    Typedef = 0x000200,
    MacroDefinition = 0x000400,
    Reference = 0x000800,
    // update stringToType when adding types
    All = 0xffffff
};
const char *nodeTypeToName(NodeType type, bool abbrev);


#endif
