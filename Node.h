#ifndef Node_h
#define Node_h

#include "Location.h"
#include "Path.h"
#include <QByteArray>

struct Node
{
    Node *parent, *nextSibling, *firstChild;
    QByteArray symbolName;
    enum Type {
        None = 0x000000,
        Root = 0x000001,
        MethodDeclaration = 0x000002,
        MethodDefinition = 0x000004,
        Reference = 0x000008,
        Class = 0x000010,
        Struct = 0x000020,
        Namespace = 0x000040,
        Variable = 0x000080,
        Enum = 0x000100,
        EnumValue = 0x000200,
        All = (None|Root|MethodDeclaration|MethodDefinition|Class|
               Struct|Namespace|Variable|Enum|EnumValue|Reference)
    } type;
    Location location;
    uint hash;

    Node();
    Node(Node *p, CXCursor c, const Location &l, uint hash);
    ~Node();
    void add(Node *child);
    void remove(Node *child);
    QByteArray toString() const;
    void print() const;
    static const char *typeToName(Type type, bool abbrev = false);

    Node *methodDeclaration() const;
    Node *methodDefinition() const;
    int size() const;
};

static inline uint qHash(const CXCursor &c, const Location &loc)
{
    QVarLengthArray<char, 128> buf(loc.path.size() + 32);
    char *buffer = buf.data();
    const int l = snprintf(buffer, buf.size() - 1, "%s%x%x%x", loc.path.constData(), loc.line, loc.column, clang_getCursorKind(c));
    return qHash(QByteArray::fromRawData(buffer, l));
}


#endif
