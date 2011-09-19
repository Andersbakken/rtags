#include "Node.h"

Node::Node()
    : parent(0), nextSibling(0), firstChild(0), type(Root)
{}

static inline bool lessThan(const Node *left, const Node *right)
{
    return (left->type < right->type || (left->type == right->type && left->symbolName < right->symbolName));
}

Node::Type Node::typeFromCursor(const CXCursor &c)
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
    default:
        break;
    }
    return Invalid;
}


Node::Node(Node *p, Type t, const CXCursor &c, const Location &l, const QByteArray &i)
    : parent(p), nextSibling(0), firstChild(0), type(t), location(l), id(i)
{
    Q_ASSERT(t != Invalid);
    Q_ASSERT(t == Root || parent);
    if (type == Reference && parent->type != Root) {
        // qDebug() << "doing parent reference" << c;
        symbolName = parent->symbolName;
    }

    if (symbolName.isEmpty())
        symbolName = eatString(clang_getCursorDisplayName(c));
   
    if (parent) {
// #ifdef QT_NO_DEBUG
        nextSibling = parent->firstChild;
        parent->firstChild = this;
// #else // ### buggy
//         if (!parent->firstChild || lessThan(this, parent->firstChild)) {
//             nextSibling = parent->firstChild;
//             parent->firstChild = this;
//         } else {
//             Node *last = parent->firstChild;
//             Node *tmp = last->nextSibling;
//             while (tmp && lessThan(tmp, this))
//                 tmp = tmp->nextSibling;
//             Q_ASSERT(last);
//             nextSibling = tmp;
//             last->nextSibling = this;
//         }
// #endif
    }
}


Node::~Node()
{
    while (firstChild) {
        Node *n = firstChild;
        n->parent = 0;
        firstChild = firstChild->nextSibling;
        delete n;
    }
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
    buf += typeToName(type);
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
    printf("%s\n", toString().constData());
    Node *child = firstChild;
    while (child) {
        child->print();
        child = child->nextSibling;
    }
}

const char *Node::typeToName(Type type, bool abbrev)
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
    case Invalid:
    case All:
        break;
    }
    Q_ASSERT(0 && "Invalid type");
    return "Invalid";
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
