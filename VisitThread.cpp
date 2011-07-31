#include "VisitThread.h"
#include <clang-c/Index.h>
#include "Node.h"

VisitThread::VisitThread()
    : QThread(0), mRoot(new Node)
{
    moveToThread(this);
}

void VisitThread::onFileParsed(const Path &path, void *u)
{
    QMutexLocker lock(&mMutex);
    QWriteLocker writeLock(&mLock);
    mFiles.insert(path);
    CXTranslationUnit unit = reinterpret_cast<CXTranslationUnit>(u);
    CXCursor cursor = clang_getTranslationUnitCursor(unit);
    clang_visitChildren(cursor, buildTree, this);
    for (QHash<uint, PendingReference>::const_iterator it = mPendingReferences.begin();
         it != mPendingReferences.end(); ++it) {
        const PendingReference &p = it.value();
        Q_ASSERT(!mNodes.contains(it.key()));
        mNodes[it.key()] = new Node(createOrGet(p.reference), p.cursor, p.location, it.key());
    }
    mPendingReferences.clear();
    clang_disposeTranslationUnit(unit);
    qDebug() << mNodes.size();
}
Node * VisitThread::createOrGet(CXCursor cursor)
{
    const CXCursorKind kind = clang_getCursorKind(cursor);
    // blacklist
    switch (kind) {
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
        if (clang_isCursorDefinition(cursor))
            break;
        // forward declaration, fall through
    case CXCursor_FirstStmt:
    case CXCursor_FirstExpr:
    case CXCursor_UnexposedDecl:
    case CXCursor_TypedefDecl:
    case CXCursor_TypeRef:
    case CXCursor_VarDecl:
    case CXCursor_DeclRefExpr:
    case CXCursor_MemberRefExpr:
    case CXCursor_ParmDecl:
    case CXCursor_UsingDirective:
    case CXCursor_NamespaceRef:
    case CXCursor_TemplateTypeParameter:
    case CXCursor_OverloadedDeclRef:
    case CXCursor_CXXBaseSpecifier:
    case CXCursor_ClassTemplate:
    case CXCursor_NonTypeTemplateParameter:
    case CXCursor_TemplateRef:
    case CXCursor_UnionDecl:
    case CXCursor_ClassTemplatePartialSpecialization:
    case CXCursor_LabelStmt:
    case CXCursor_LabelRef:
    case CXCursor_UsingDeclaration:
    case CXCursor_TemplateTemplateParameter:
        return createOrGet(clang_getCursorSemanticParent(cursor));
    case CXCursor_TranslationUnit:
        return mRoot;
    default:
        break;
    }
    if (clang_isInvalid(kind))
        return mRoot;
    const Location location(cursor);
    if (!location.exists())
        return createOrGet(clang_getCursorSemanticParent(cursor));

    const uint hash = qHash(cursor, location);
    if (kind == CXCursor_CallExpr || kind == CXCursor_MemberRef) {
        if (mNodes.contains(hash))
            return mNodes.value(hash);
        CXCursor ref = clang_getCursorReferenced(cursor);
        if (isValidCursor(ref) && !mPendingReferences.contains(hash)) {
            const PendingReference p = { cursor, ref, location };
            mPendingReferences[hash] = p;
            Q_ASSERT(!clang_equalCursors(ref, cursor));
        }
        return createOrGet(clang_getCursorSemanticParent(cursor));
    }

    Node *&node = mNodes[hash];
    if (!node) {
        node = new Node(createOrGet(clang_getCursorSemanticParent(cursor)), cursor, location, hash);
        Q_ASSERT(node->parent);
    }
    return node;
}

CXChildVisitResult VisitThread::buildTree(CXCursor cursor, CXCursor, CXClientData data)
{
    VisitThread *visitThread = reinterpret_cast<VisitThread*>(data);
    visitThread->createOrGet(cursor);
    return CXChildVisit_Recurse;
}

static int recursiveDelete(Node *node, QHash<unsigned, Node*> &nodes)
{
    int ret = 1;
    for (Node *c = node->firstChild; c; c = c->nextSibling)
        ret += recursiveDelete(c, nodes);

    node->firstChild = 0;
    Q_ASSERT(nodes.contains(node->hash));
    nodes.remove(node->hash);
    delete node;
    return ret;
}

static int removeChildren(Node *node, const Path &path, QHash<unsigned, Node*> &nodes)
{
    Node *prev = 0;
    Node *child = node->firstChild;
    int ret = 0;
    while (child) {
        if (child->location.path == path) {
            if (!prev) {
                node->firstChild = child->nextSibling;
                ret += recursiveDelete(child, nodes);
                child = node->firstChild;
            } else {
                prev->nextSibling = child->nextSibling;
                ret += recursiveDelete(child, nodes);
                child = prev->nextSibling;
            }
        } else {
            removeChildren(child, path, nodes);
            prev = child;
            child = child->nextSibling;
        }
    }
    return ret;
}

void VisitThread::invalidate(const Path &path)
{
    {
        QMutexLocker lock(&mMutex);
        mFiles.remove(path);
    }
    QWriteLocker writeLock(&mLock);
    const int count = removeChildren(mRoot, path, mNodes);
    qDebug("Removed %d nodes %d", count, mNodes.size());
}
void VisitThread::printTree()
{
    QReadLocker lock(&mLock);
    mRoot->print();
}

static bool match(const QByteArray &qualifiedSymbolName, const QRegExp &rx)
{
    return QString::fromLocal8Bit(qualifiedSymbolName).contains(rx);
}

static bool match(const QByteArray &qualifiedSymbolName, const QByteArray &match)
{
    return qualifiedSymbolName.contains(match);
}

template <typename T>
static int recurse(const T &t, QByteArray path, const Node *node, uint nodeTypes,
                   HandleResult handler, void *userdata)
{
    Q_ASSERT(node);
    int ret = 0;
    if (node->type & nodeTypes) {
        const QByteArray full = path + node->symbolName;
        // ### this could maybe be optimized to reuse the same buffer again and again
        if (match(full, t)) {
            handler(node, full, userdata);
            ++ret;
        }
    }
    switch (node->type) {
    case Node::Namespace:
    case Node::Class:
    case Node::Struct:
        path.append(node->symbolName + "::");
    default:
        break;
    }
    // ### could consider short circuiting here if for example we know this node
    // ### only has MethodReference children and !(types & MethodReference) || !ret
    for (Node *c = node->firstChild; c; c = c->nextSibling) {
        ret += recurse(t, path, c, nodeTypes, handler, userdata);
    }
    return ret;
}

int VisitThread::lookup(const QByteArray &pattern, uint flags, uint nodeTypes, HandleResult handler, void *userdata)
{
    QReadLocker lock(&mLock);
    Q_ASSERT(handler);
    if (flags & RegExp) {
        QRegExp rx(pattern);
        return recurse(rx, QByteArray(), mRoot, nodeTypes, handler, userdata);
    } else {
        return recurse(pattern, QByteArray(), mRoot, nodeTypes, handler, userdata);
    }
}

QSet<Path> VisitThread::files() const
{
    QMutexLocker lock(&mMutex);
    return mFiles;
}

void VisitThread::run()
{
    Path::initStaticData();
    exec();
}
