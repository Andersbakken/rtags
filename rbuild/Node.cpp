#include "Node.h"

QMap<QByteArray, Node*> Node::sNodes;
int32_t Node::sLongestId = 0;

Node::Node()
    : parent(0), nextSibling(0), firstChild(0), containingFunction(0), type(Root), symbolName("RootNode")
{
}

Node::Node(Node *p, NodeType t, const QByteArray &symbol, const Location &l, const QByteArray &i)
    : parent(p), nextSibling(0), firstChild(0), containingFunction(0), type(t), location(l), id(i), symbolName(symbol)
{
    Q_ASSERT(!l.path.isEmpty());
    Q_ASSERT(!sNodes.contains(id));
    Q_ASSERT(id == l.toString());
    sNodes[id] = this;
    sLongestId = qMax(sLongestId, id.size());
    Q_ASSERT(t != Invalid);
    Q_ASSERT(t == Root || parent);
    Q_ASSERT(type != Reference || parent->type != Root);

    if (parent) {
        nextSibling = parent->firstChild;
        parent->firstChild = this;
        if (parent->type == Reference) {
            qWarning() << parent->toString() << "should not have children, getting this one" << toString();
        }
        Q_ASSERT(parent->type != Reference);
    }
}

Node::~Node()
{
    if (!id.isEmpty()) {
        const int removed = sNodes.remove(id);
        (void)removed;
        Q_ASSERT(removed > 0);
    }
    while (firstChild) {
        Node *n = firstChild;
        n->parent = 0;
        firstChild = firstChild->nextSibling;
        delete n;
    }
}
static inline bool lessThan(const Node *left, const Node *right)
{
    return (left->type < right->type || (left->type == right->type && left->symbolName < right->symbolName));
}

NodeType Node::nodeTypeFromCursor(const CXCursor &c)
{
    const CXCursorKind kind = clang_getCursorKind(c);
    switch (kind) {
    case CXCursor_TypedefDecl:
        return Typedef;
    case CXCursor_StructDecl:
        return clang_isCursorDefinition(c) ? Struct : Reference;
        break;
    case CXCursor_ClassDecl:
        return clang_isCursorDefinition(c) ? Class : Reference;
        break;
    case CXCursor_MemberRefExpr:
    case CXCursor_TypeRef:
    case CXCursor_MemberRef:
    case CXCursor_DeclRefExpr:
    case CXCursor_MacroExpansion:
        return Reference;
        break;
    case CXCursor_FieldDecl:
    case CXCursor_VarDecl:
    case CXCursor_ParmDecl:
        return Variable;
        break;
    case CXCursor_CXXMethod:
    case CXCursor_FunctionDecl:
    case CXCursor_Constructor:
    case CXCursor_Destructor:
    case CXCursor_FunctionTemplate:
    case CXCursor_ConversionFunction:
        return clang_isCursorDefinition(c) ? MethodDefinition : MethodDeclaration;
        break;
    case CXCursor_Namespace:
        return Namespace;
        break;
    case CXCursor_EnumDecl:
        return Enum;
        break;
    case CXCursor_EnumConstantDecl:
        return EnumValue;
        break;
    case CXCursor_MacroDefinition:
        return MacroDefinition;
    default:
        break;
    }
    return Invalid;
}

QByteArray Node::toString() const
{
    if (type == Root)
        return "Root";
    int indent = 0; // ### ugly
    for (Node *p=parent; p; p = p->parent) {
        indent += 2;
    }
    QByteArray buf(indent, ' ');
    buf += nodeTypeToName(type, Normal);
    buf += ' ';
    buf += symbolName;
    buf += " [";
    buf += location.path;
    buf += ':';
    buf += QByteArray::number(location.line);
    buf += ':';
    buf += QByteArray::number(location.column);
    buf += ']';
    return buf;
}

void Node::print() const
{
    qDebug("%s", toString().constData());
    Node *child = firstChild;
    while (child) {
        child->print();
        child = child->nextSibling;
    }
}

int Node::size() const
{
    int ret = sizeof(*this);
    ret += symbolName.capacity() + 1;
    ret += location.path.capacity() + 1;
    for (Node *n=firstChild; n; n = n->nextSibling) {
        ret += n->size();
    }
    return ret;
}

Node *Node::methodDeclaration() const
{
    switch (type) {
    case Reference:
        Q_ASSERT(parent && parent->type == MethodDefinition);
        return parent->methodDeclaration();
    case MethodDeclaration:
        return const_cast<Node*>(this);
    case MethodDefinition:
        Q_ASSERT(parent);
        for (Node *n = parent->firstChild; n; n = n->nextSibling) {
            if (n->type == MethodDeclaration && n->symbolName == symbolName)
                return n;

        }
        return 0;
    default:
        break;
    }
    Q_ASSERT(0 && "This doesn't make any sense");
    return 0;
}

Node *Node::methodDefinition() const
{
    switch (type) {
    case Reference:
        Q_ASSERT(parent && parent->type == MethodDefinition);
        return parent;
    case MethodDefinition:
        return const_cast<Node*>(this);
    case MethodDeclaration:
        Q_ASSERT(parent);
        for (Node *n = parent->firstChild; n; n = n->nextSibling) {
            if (n->type == MethodDefinition && n->symbolName == symbolName)
                return n;
        }
        qDebug() << "somehow not finding definition for" << symbolName;
        return 0;
    default:
        break;
    }
    Q_ASSERT(0 && "This doesn't make any sense");
    return 0;
}
