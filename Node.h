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
    } type;
    Location location;
    QByteArray id; // ### we don't really need to store this

    Node();
    Node(Node *p, Type t, const CXCursor &c, const Location &l, const QByteArray &id);
    ~Node();
    static Type typeFromCursor(const CXCursor &c);
    QByteArray toString() const;
    void print() const;
    static const char *typeToName(Type type, bool abbrev = false);

    Node *methodDeclaration() const;
    Node *methodDefinition() const;
    int size() const;
    static QMap<QByteArray, Node*> *sNodes;
};

struct NodeData {
    char symbolName[128]; // ### is this enough?
    quint8 type, column;
    quint16 fileId, line, index, childCount; // which child am I
};

#endif
