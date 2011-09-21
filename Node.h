#ifndef Node_h
#define Node_h

#include "Location.h"
#include "Path.h"
#include <QByteArray>
#include "NodeType.h"

struct Node
{
    Node *parent, *nextSibling, *firstChild;
    QByteArray symbolName;
    NodeType type;
    Location location;
    QByteArray id; // ### we don't really need to store this

    Node();
    Node(Node *p, NodeType t, const CXCursor &c, const Location &l, const QByteArray &id);
    ~Node();
    static NodeType typeFromCursor(const CXCursor &c);
    QByteArray toString() const;
    void print() const;

    Node *methodDeclaration() const;
    Node *methodDefinition() const;
    int size() const;
    static QMap<QByteArray, Node*> *sNodes;
};

#endif
