#include "VisitThread.h"
#include <clang-c/Index.h>
#include "Node.h"

VisitThread::VisitThread()
    : QThread(0), mRoot(new Node), mMutex(QMutex::Recursive), mQuitting(false),
      mLongestId(0)
{
    Node::sNodes = &mNodes;
    setObjectName("VisitThread");
    moveToThread(this);
}

VisitThread::~VisitThread()
{
    Node::sNodes = 0;
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
        fflush(stdout);
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
    CXSourceLocation location = clang_getCursorLocation(cursor);
    CXFile file = 0;
    clang_getInstantiationLocation(location, &file, 0, 0, 0);
    // ### is this safe?
    if (!file)
        return CXChildVisit_Continue;

#warning needs to short circuit when encounting a header it has seen
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
                   << "\ncursorId(parent)" << Location(parent).toString()
                   << "\ncursorId(u->lastCursor)" << Location(u->lastCursor).toString();
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
    if (clang_getCursorKind(c->cursor) == CXCursor_MacroExpansion) {
        const Location loc(c->cursor);
        Q_ASSERT(!loc.isNull());
        const QByteArray symbolName = eatString(clang_getCursorSpelling(c->cursor));
        for (CursorNode *cn = c->parent->firstChild; cn; cn = cn->nextSibling) {
            if (clang_getCursorKind(cn->cursor) == CXCursor_MacroDefinition
                && symbolName == eatString(clang_getCursorSpelling(cn->cursor))) {
                const QByteArray macroDefinitionId = Location(cn->cursor).toString();
                Node *parent = mNodes.value(macroDefinitionId);
                Q_ASSERT(parent);
                const QByteArray id = loc.toString();
                qint32 old = mLongestId;
                mLongestId = qMax(id.size(), mLongestId);
                if (old != mLongestId) {
                    qDebug() << "new longest" << id << old << mLongestId;
                }
                mNodes[id] = new Node(parent, Reference, c->cursor, loc, id);
                return;
            }
        }
    }

    const NodeType type = Node::nodeTypeFromCursor(c->cursor);
    if (type == Reference) {
        const Location loc(c->cursor);
        if (loc.exists()) {
            const QByteArray id = loc.toString();
            if (!mNodes.contains(id)) {
                const PendingReference r = { c, loc };
                references[id] = r;
            }
        }
    } else {
        if (c->parent && type != Invalid) {
            const Location loc(c->cursor);
            if (loc.exists()) {
                const QByteArray id = loc.toString();
                Node *&node = mNodes[id];
                if (node)
                    return; // we've seen this whole branch
                // ### may not need to do this for all types of nodes
                CXCursor realParent = clang_getCursorSemanticParent(c->cursor);
                if (isValidCursor(realParent) && !clang_equalCursors(realParent, c->parent->cursor)) {
                    const QByteArray parentId = Location(realParent).toString();
                    parent = mNodes.value(parentId, parent);
                }

                node = new Node(parent, type, c->cursor, loc, id);
                qint32 old = mLongestId;
                mLongestId = qMax(id.size(), mLongestId);
                if (old != mLongestId) {
                    qDebug() << "new longest" << id << old << mLongestId;
                }
                
                parent = node;
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
                   << mNodes.value(id)->symbolName << nodeTypeToName(mNodes.value(id)->type)
                   << mNodes.value(id)->location;
        return;
    }
    if (Node::nodeTypeFromCursor(c->cursor) != Invalid && loc.exists()) {
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
        const QByteArray refId = Location(ref).toString();
        Node *refNode = mNodes.value(refId);
        if (!refNode) {
            // qWarning() << "Can't find referenced node" << c->cursor << ref << refId;
            return;
        }
        if (refNode->type == MethodDefinition) {
            Node *decl = refNode->methodDeclaration();
            if (decl)
                refNode = decl;
        }
        mNodes[id] = new Node(refNode, Reference, c->cursor, loc, id);
        qint32 old = mLongestId;
        mLongestId = qMax(id.size(), mLongestId);
        if (old != mLongestId) {
            qDebug() << "new longest" << id << old << mLongestId;
        }
        
    }

    for (CursorNode *child=c->firstChild; child; child = child->nextSibling) {
        const Location l(child->cursor);
        addReference(child, l.toString(), l);
        // if (typeFromCursor(child->cursor) != Invalid) {
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
#ifndef QT_NO_DEBUG
        const QByteArray dump = qgetenv("RTAGS_DUMP");
        if (dump == "1" || dump.contains(path.fileName())) {
            ud.root->dump(0);
            printf("Tree done\n");
            fflush(stdout);
            sleep(1);
        }
#endif
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
    printf("%s %d: clang_disposeTranslationUnit(unit);\n", __FILE__, __LINE__);
    if (!save("/tmp/balle2"))
        printf("%s %d: if (!save(\"/tmp/balle\"))\n", __FILE__, __LINE__);
}

static void removeChildren(Node *node, const QSet<Path> &paths)
{
    Node *prev = 0;
    Node *child = node->firstChild;
    while (child) {
        if (paths.contains(child->location.path)) {
            Node *next = child->nextSibling;
            Q_ASSERT(!prev == (node->firstChild == child));
            if (node->firstChild == child) {
                node->firstChild = next;
            } else {
                prev->nextSibling = next;
            }
            delete child;
            child = next;
        } else {
            removeChildren(child, paths);
            prev = child;
            child = child->nextSibling;
        }
    }
}

void VisitThread::invalidate(const QSet<Path> &paths)
{
    Q_ASSERT(!paths.isEmpty());
    QMutexLocker lock(&mMutex);
    const int oldCount = mNodes.size();
    Q_UNUSED(oldCount);
    const int oldSize = mRoot->size();
    Q_UNUSED(oldSize);
    removeChildren(mRoot, paths);
#ifndef QT_NO_DEBUG
    QByteArray out;
    foreach(const Path &path, paths) {
        out += path + ' ';
    }
    out.chop(1);
    const int currentSize = mRoot->size();
    qDebug("Removed %d nodes (%d) and %d bytes (%d) for %s",
           oldCount - mNodes.size(), mNodes.size(),
           oldSize - currentSize, currentSize, out.constData());
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
    case Namespace:
    case Class:
    case Struct:
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
Node * VisitThread::nodeForLocation(const Location &loc) const
{
    QMutexLocker lock(&mMutex);
    return mNodes.value(loc.toString());
}

static int nodeSize(Node *node)
{
    // zero termination for each string => 2
    //
    return (sizeof(qint32) /* type */
            + sizeof(qint32) /* Location pos */
            + sizeof(qint32) /* parent pos */
            + sizeof(qint32) /* nextSibling pos */
            + sizeof(qint32) /* firstChild pos */
            + node->symbolName.size() + 1); /*symbolName*/
}

// Format
// 2 * char => Rt
// qint32 => number of Nodes
// qint32 => length of each id
// first id ...
// [qint32][location padded to length]

enum { FirstId = (sizeof(char) * 2) + sizeof(qint32) + sizeof(qint32) };
static qint32 writeNode(QIODevice *device, Node *node, const QHash<Node*, qint32> &positions,
                        int entryIdx, int entryLength)
{
    const qint32 nodePosition = positions.value(node, -1);
    Q_ASSERT(nodePosition > 0);
    device->seek(nodePosition);
    const qint32 type = node->type;
    qDebug() << "writing type at" << device->pos() << type << "for" << node->symbolName << nodeTypeToName(node->type);
    device->write(reinterpret_cast<const char*>(&type), sizeof(qint32));
    const qint32 location = (entryIdx * entryLength) + FirstId + sizeof(qint32);
    device->write(reinterpret_cast<const char*>(&location), sizeof(qint32)); // pointer to where the location sits in the index
    const qint32 parent = positions.value(node->parent, 0);
    device->write(reinterpret_cast<const char*>(&parent), sizeof(qint32));
    const qint32 nextSibling = positions.value(node->nextSibling, 0);
    device->write(reinterpret_cast<const char*>(&nextSibling), sizeof(qint32));
    const qint32 firstChild = positions.value(node->firstChild, 0);
    device->write(reinterpret_cast<const char*>(&firstChild), sizeof(qint32));
    device->write(node->symbolName);
    device->write('\0');
    return nodePosition;
}

bool VisitThread::save(const QByteArray &path)
{
    QFile file(path);
    if (!file.open(QIODevice::WriteOnly)) {
        return false;
    }
    
    // ### error checking?
    QMutexLocker lock(&mMutex);
    const qint32 nodeCount = mNodes.size();
    const int entryLength = mLongestId + 1 + sizeof(qint32);
    QByteArray header(FirstId + entryLength * nodeCount, '\0');
    int pos = header.size();
    QHash<Node*, qint32> positions;
    positions[mRoot] = pos;
    pos += nodeSize(mRoot);
    for (QMap<QByteArray, Node*>::const_iterator it = mNodes.begin(); it != mNodes.end(); ++it) {
        positions[it.value()] = pos;
        pos += nodeSize(it.value());
    }
    file.resize(pos);
    // device->write("Rt", 2);
    char *out = header.data();
    *out++ = 'R';
    *out++ = 't';
    memcpy(out, reinterpret_cast<const char *>(&nodeCount), sizeof(qint32));
    out += sizeof(qint32);
    memcpy(out, reinterpret_cast<const char *>(&mLongestId), sizeof(qint32));
    out += sizeof(qint32);
    int entry = 0;
    qWarning() << "entryLength" << entryLength << mLongestId;
    for (QMap<QByteArray, Node*>::const_iterator it = mNodes.begin(); it != mNodes.end(); ++it) {
        Node *node = it.value();
        const QByteArray &key = it.key();
        qint32 nodePosition = positions.value(node, -1);
        Q_ASSERT(nodePosition > 0);
        file.seek(nodePosition);
        memcpy(out, reinterpret_cast<const char *>(&nodePosition), sizeof(qint32));
        strncpy(out + sizeof(qint32), key.constData(), key.size());
        out += entryLength;
        qint32 type = node->type;
        qDebug() << "writing type at" << file.pos() << type << "for" << node->symbolName << nodeTypeToName(node->type);
        file.write(reinterpret_cast<const char*>(&type), sizeof(qint32));
        const qint32 location = (entry++ * entryLength) + FirstId + sizeof(qint32);
        file.write(reinterpret_cast<const char*>(&location), sizeof(qint32)); // pointer to where the location sits in the index
        const qint32 parent = positions.value(node->parent, 0);
        file.write(reinterpret_cast<const char*>(&parent), sizeof(qint32));
        const qint32 nextSibling = positions.value(node->nextSibling, 0);
        file.write(reinterpret_cast<const char*>(&nextSibling), sizeof(qint32));
        const qint32 firstChild = positions.value(node->firstChild, 0);
        file.write(reinterpret_cast<const char*>(&firstChild), sizeof(qint32));
        file.write(node->symbolName);
        file.write('\0');
    }
    file.seek(0);
    file.write(header);
    for (int i=0; i<FirstId; ++i) {
        printf("%d: 0x%x %c\n", i, header.at(i), header.at(i));
    }
    int p = FirstId;
    for (int i=0; i<nodeCount; ++i) {
        for (int j=0; j<entryLength; ++j) {
            char ch = header.at(p++);
            if (ch == '\0') {
                printf("_");
            } else if (ch < 32) {
                printf("-");
            } else {
                printf("%c", ch);
            }
        }
        printf("\n");
    }

    // device->
    return true;
}
