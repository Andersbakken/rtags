#include "VisitThread.h"
#include <clang-c/Index.h>
#include "Node.h"

int tot = 0;
QList<CXCursor> all;

VisitThread::VisitThread()
    : QThread(0), mRoot(new Node), mQuitting(false)
{
    mBytes = mRoot->size();
    setObjectName("VisitThread");
    moveToThread(this);
}

void VisitThread::abort()
{
    mQuitting = true;
    quit();
}

struct CursorNode {
    CursorNode(CXCursor c, CursorNode *p = 0)
        : cursor(c), parent(p), firstChild(0), nextSibling(0), lastChild(0)
    {
        if (p) {
            p->append(this);
        }
    }
    ~CursorNode()
    {
        CursorNode *c = firstChild;
        while (c) {
            CursorNode *tmp = c;
            c = c->nextSibling;
            delete tmp;
        }
    }
    int count() const
    {
        int ret = 1;
        for (CursorNode *c=firstChild; c; c = c->nextSibling) {
            ret += c->count();
        }
        return ret;
    }
    CXCursor cursor;
    CursorNode *parent, *firstChild, *nextSibling, *lastChild;
    void dump(int indent)
    {
        for (int i=0; i<indent; ++i) {
            printf(" ");
        }

        QString str;
        QDebug(&str) << cursor;
        str.remove("\"");
        printf("%s\n", str.toLocal8Bit().constData());
        for (CursorNode *c=firstChild; c; c = c->nextSibling) {
            c->dump(indent + 2);
        }
    }
    void append(CursorNode *c)
    {
        c->parent = this;
        if (lastChild) {
            lastChild->nextSibling = c;
            lastChild = c;
        } else {
            lastChild = firstChild = c;
        }
        nextSibling = 0;
    }
};
struct ComprehensiveTreeUserData {
    CursorNode *root;
    CursorNode *last;
    QVector<QPair<CXCursor, CursorNode*> > parents;
    CXCursor lastCursor;
};

/* There's a reason we don't use clang_equalCursors. It occasionally seems to
 * return 0 when the cursors seemingly are equal
 */

static bool operator==(const CXCursor &left, const CXCursor &right)
{
    return (left.kind == right.kind
            && clang_equalLocations(clang_getCursorLocation(left),
                                    clang_getCursorLocation(right)));
}

CXChildVisitResult buildComprehensiveTree(CXCursor cursor, CXCursor parent, CXClientData data)
{
    // qDebug() << cursor << parent;
    ComprehensiveTreeUserData *u = reinterpret_cast<ComprehensiveTreeUserData*>(data);
    CursorNode *p = 0;
    if (!u->root) {
        u->root = new CursorNode(parent);
        p = u->root;
        u->parents.append(qMakePair(parent, u->root));
        u->lastCursor = cursor;
    } else {
        Q_ASSERT(u->last);
        if (parent == u->lastCursor) {
            p = u->last;
            Q_ASSERT(p);
            u->parents.append(qMakePair(parent, p));
        } else {
            for (int i=u->parents.size() - 1; i>=0; --i) {
                if (parent == u->parents.at(i).first) {
                    p = u->parents.at(i).second;
                    u->parents.resize(i + 1);
                    break;
                }
            }
        }
    }
    if (!p) {
        qDebug() << parent.kind << u->lastCursor.kind
                 << parent.data[0] << u->lastCursor.data[0]
                 << parent.data[1] << u->lastCursor.data[1]
                 << parent.data[2] << u->lastCursor.data[2];
        
        qWarning() << "crashing cursor is" << cursor
                   << "\nparent is" << parent
                   << "\nlastCursor is" << u->lastCursor
            // << "\nparents are" << u->parents
                   << "\nparent and lastCursor are equal" << clang_equalCursors(parent, u->lastCursor)
                   << "\ncursorId(parent)" << cursorId(parent)
                   << "\ncursorId(u->lastCursor)" << cursorId(u->lastCursor);

        // qWarning
        // qWarning() << u->root->cursor << clang_equalCursors(u->root->cursor, parent)
        //            << clang_equalCursors(parent, u->last);
        // u->root->dump(0);
    }
    Q_ASSERT(p);
    u->last = new CursorNode(cursor, p);
    u->lastCursor = cursor;
    switch (clang_getCursorKind(cursor)) {
    case CXCursor_EnumConstantDecl:
    case CXCursor_MemberRefExpr:
    case CXCursor_DeclRefExpr:
        return CXChildVisit_Continue; //
    default:
        break;
    }
    return CXChildVisit_Recurse;
}

void VisitThread::buildTree(Node *parent, CursorNode *c, QHash<QByteArray, PendingReference> &references)
{
    const Node::Type type = Node::typeFromCursor(c->cursor);
    if (type == Node::Reference) {
        const Location loc(c->cursor);
        if (loc.exists()) {
            const QByteArray id = cursorId(c->cursor, loc);
            if (!mNodes.contains(id)) {
                const PendingReference r = { c, loc };
                references[id] = r;
            }
        }
    } else {
        if (c->parent && type != Node::Invalid) {
            const Location loc(c->cursor);
            if (loc.exists()) {
                const QByteArray id = cursorId(c->cursor, loc);
                Node *&node = mNodes[id];
                if (node)
                    return; // we've seen this whole branch
                parent = node = new Node(parent, type, c->cursor, loc, id);
            }
        }
        for (CursorNode *child=c->firstChild; child; child = child->nextSibling) {
            buildTree(parent, child, references);
        }
    }
}

void VisitThread::addReference(CursorNode *c, const QByteArray &id, const Location &loc)
{
    if (mNodes.contains(id)) {
        qWarning() << "Turns out" << c->cursor << "already exists"
                   << mNodes.value(id)->symbolName << Node::typeToName(mNodes.value(id)->type)
                   << mNodes.value(id)->location;
        return;
    }
    if (Node::typeFromCursor(c->cursor) != Node::Invalid && loc.exists()) {
        const CXCursorKind kind = clang_getCursorKind(c->cursor);

        CXCursor ref = clang_getCursorReferenced(c->cursor);
        if (clang_equalCursors(ref, c->cursor) && (kind == CXCursor_ClassDecl || kind == CXCursor_StructDecl)) { // ### namespace too?
            ref = clang_getCursorDefinition(ref);
        }

        if (!isValidCursor(ref)) {
            if (/*kind != CXCursor_CallExpr && */kind != CXCursor_ClassDecl && kind != CXCursor_StructDecl)
                qWarning() << "Can't get valid cursor for" << c->cursor << "child of" << c->parent->cursor;
            return;
        }

        const CXCursorKind refKind = clang_getCursorKind(ref);
        if (kind == CXCursor_DeclRefExpr) {
            switch (refKind) {
            case CXCursor_ParmDecl:
            case CXCursor_VarDecl:
            case CXCursor_FieldDecl:
            case CXCursor_CXXMethod:
            case CXCursor_EnumConstantDecl:
            case CXCursor_FunctionDecl:
                break;
            case CXCursor_NonTypeTemplateParameter:
                return;
            default:
                qDebug() << "throwing out this pending CXCursor_DeclRefExpr" << c->cursor << ref;
                return;
            }
        }
        const QByteArray refId = cursorId(ref);
        Node *refNode = mNodes.value(refId);
        if (!refNode) {
            // qWarning() << "Can't find referenced node" << c->cursor << ref << refId;
            return;
        }
        mNodes[id] = new Node(refNode, Node::Reference, c->cursor, loc, id);
    }

    for (CursorNode *child=c->firstChild; child; child = child->nextSibling) {
        const Location l(child->cursor);
        addReference(child, cursorId(child->cursor, l), l);
        // if (Node::typeFromCursor(child->cursor) != Node::Invalid) {
        //     qWarning() << "This node is a child of a ref" << child->cursor
        //                << c->cursor << ref;
        // }
    }
    
}

void VisitThread::onFileParsed(const Path &path, void *u)
{
    CXTranslationUnit unit = reinterpret_cast<CXTranslationUnit>(u);
    if (!mQuitting) {
        CXCursor rootCursor = clang_getTranslationUnitCursor(unit);
        ComprehensiveTreeUserData ud;
        ud.last = ud.root = 0;
        ud.lastCursor = clang_getNullCursor();
        clang_visitChildren(rootCursor, buildComprehensiveTree, &ud);
        if (qgetenv("RTAGS_DUMP").contains(path.fileName()))
            ud.root->dump(0);
        QHash<QByteArray, PendingReference> references;
        QMutexLocker lock(&mMutex);
        const int old = mNodes.size();
        buildTree(mRoot, ud.root, references);
        for (QHash<QByteArray, PendingReference>::const_iterator it = references.begin(); it != references.end(); ++it) {
            const PendingReference &p = it.value();
            addReference(p.node, it.key(), p.location);
        }
        delete ud.root;
        qDebug() << "added" << (mNodes.size() - old) << "nodes for" << path << ". Total" << mNodes.size();
    }
    clang_disposeTranslationUnit(unit);
}

static int recursiveDelete(Node *node, QHash<QByteArray, Node*> &nodes, int &size)
{
    int ret = 1;
    for (Node *c = node->firstChild; c; c = c->nextSibling)
        ret += recursiveDelete(c, nodes, size);

    node->firstChild = 0;
    Q_ASSERT(nodes.contains(node->id));
    nodes.remove(node->id);
    size -= node->size();
    delete node;
    return ret;
}

static int removeChildren(Node *node, const QSet<Path> &paths, QHash<QByteArray, Node*> &nodes, int &mSize)
{
    Node *prev = 0;
    Node *child = node->firstChild;
    int ret = 0;
    while (child) {
        if (paths.contains(child->location.path)) {
            Node *next = child->nextSibling;
            qDebug() << "Found a node" << child->location.path << child << child->symbolName
                     << Node::typeToName(child->type);
            ret += recursiveDelete(child, nodes, mSize);
            Q_ASSERT(!prev == (node->firstChild == child));
            if (node->firstChild == child) {
                node->firstChild = next;
            } else {
                prev->nextSibling = next;
            }
            child = next;
        } else {
            removeChildren(child, paths, nodes, mSize);
            prev = child;
            child = child->nextSibling;
        }
    }
    return ret;
}

void VisitThread::invalidate(const QSet<Path> &paths)
{
    Q_ASSERT(!paths.isEmpty());
    QMutexLocker writeLock(&mMutex);
    const int old = mBytes;
    const int count = removeChildren(mRoot, paths, mNodes, mBytes);
#ifndef QT_NO_DEBUG
    QByteArray out;
    foreach(const Path &path, paths) {
        out += path + ' ';
    }
    out.chop(1);
    qDebug("Removed %d nodes %d (removed %d bytes, current %d bytes) for %s",
           count, mNodes.size(), old - mBytes, mBytes, out.constData());
#endif
}
void VisitThread::printTree()
{
    QMutexLocker lock(&mMutex);
    mRoot->print();
}

static Match::MatchResult recurse(Match *match, const Node *node, QByteArray path)
{
    Q_ASSERT(match);
    Q_ASSERT(node);
    Match::MatchResult result = Match::Recurse;
    if (node->type & match->nodeTypes)
        result = match->match(path, node);

    switch (node->type) {
    case Node::Namespace:
    case Node::Class:
    case Node::Struct:
        path.append(node->symbolName + "::");
        break;
    default:
        break;
    }
    // ### could consider short circuiting here if for example we know this node
    // ### only has MethodReference children and !(types & MethodReference) || !ret
    if (result == Match::Recurse) {
        for (Node *c = node->firstChild; c; c = c->nextSibling) {
            if (recurse(match, c, path) == Match::Finish) {
                result = Match::Finish;
                break;
            }
        }
    }
    return result;
}

void VisitThread::lookup(Match *match)
{
    QMutexLocker lock(&mMutex);
    Q_ASSERT(match);
    recurse(match, mRoot, QByteArray());
}
