#ifndef Node_h
#define Node_h

#include "Location.h"
#include "Path.h"
#include <QByteArray>
#include "Shared.h"

struct Node
{
    Node *parent, *nextSibling, *firstChild, *containingFunction;
    const NodeType type;
    const Location location;
    const QByteArray id; // ### we don't really need to store this
    QByteArray symbolName;

    Node();
    Node(Node *p, NodeType t, const QByteArray &symbolName, const Location &l, const QByteArray &id);
    ~Node();
    static NodeType nodeTypeFromCursor(const CXCursor &c);
    QByteArray toString() const;
    void print() const;

    Node *methodDeclaration() const;
    Node *methodDefinition() const;
    int size() const;
    static QMap<QByteArray, Node*> sNodes;
    static int32_t sLongestId;
};

#endif
