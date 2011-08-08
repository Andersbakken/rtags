#include "VisitThread.h"
#include <clang-c/Index.h>
#include "Node.h"

VisitThread::VisitThread()
    : QThread(0), mRoot(new Node)
{
    mBytes = mRoot->size();
    setObjectName("VisitThread");
    moveToThread(this);
}

void VisitThread::onFileParsed(const Path &path, void *u)
{
    QMutexLocker lock(&mMutex);
    QWriteLocker writeLock(&mLock);
    const int old = mNodes.size();
    mFiles.insert(path);
    CXTranslationUnit unit = reinterpret_cast<CXTranslationUnit>(u);
    CXCursor cursor = clang_getTranslationUnitCursor(unit);
    clang_visitChildren(cursor, buildTree, this);
    int added = 0;
    for (QHash<uint, PendingReference>::const_iterator it = mPendingReferences.begin();
         it != mPendingReferences.end(); ++it) {
        const PendingReference &p = it.value();
        Q_ASSERT(!mNodes.contains(it.key()));
        Node *n = new Node(createOrGet(p.reference), p.cursor, p.location, it.key());
        const int s = n->size();
        added += s;
        mBytes += s;
        mNodes[it.key()] = n;
    }
    mPendingReferences.clear();
    clang_disposeTranslationUnit(unit);
    qDebug() << mNodes.size() - old << "nodes added for" << path << added << "bytes added total is" << mBytes;
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
    case CXCursor_ObjCSuperClassRef:
    case CXCursor_ObjCInterfaceDecl:
    case CXCursor_ObjCCategoryDecl:
    case CXCursor_ObjCProtocolDecl:
    case CXCursor_ObjCPropertyDecl:
    case CXCursor_ObjCIvarDecl:
    case CXCursor_ObjCInstanceMethodDecl:
    case CXCursor_ObjCClassMethodDecl:
    case CXCursor_ObjCImplementationDecl:
    case CXCursor_ObjCCategoryImplDecl:
    case CXCursor_ObjCSynthesizeDecl:
    case CXCursor_ObjCDynamicDecl:
    case CXCursor_ObjCProtocolRef:
    case CXCursor_ObjCClassRef:
    case CXCursor_ObjCMessageExpr:
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
        if (Node *n = mNodes.value(hash))
            return n;
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

static int recursiveDelete(Node *node, QHash<unsigned, Node*> &nodes, int &size)
{
    int ret = 1;
    for (Node *c = node->firstChild; c; c = c->nextSibling)
        ret += recursiveDelete(c, nodes, size);

    node->firstChild = 0;
    Q_ASSERT(nodes.contains(node->hash));
    nodes.remove(node->hash);
    size -= node->size();
    delete node;
    return ret;
}

static int removeChildren(Node *node, const Path &path, QHash<unsigned, Node*> &nodes, int &mSize)
{
    Node *prev = 0;
    Node *child = node->firstChild;
    int ret = 0;
    while (child) {
        if (child->location.path == path) {
            if (!prev) {
                node->firstChild = child->nextSibling;
                ret += recursiveDelete(child, nodes, mSize);
                child = node->firstChild;
            } else {
                prev->nextSibling = child->nextSibling;
                ret += recursiveDelete(child, nodes, mSize);
                child = prev->nextSibling;
            }
        } else {
            removeChildren(child, path, nodes, mSize);
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
    const int old = mBytes;
    const int count = removeChildren(mRoot, path, mNodes, mBytes);
    qDebug("Removed %d nodes %d (removed %d bytes, current %d bytes) for %s",
           count, mNodes.size(), old - mBytes, mBytes, path.constData());
}
void VisitThread::printTree()
{
    QReadLocker lock(&mLock);
    mRoot->print();
}

static bool match(const QByteArray &name, const QList<QRegExp> &rxs)
{
    QString str = QString::fromLocal8Bit(name);
    foreach(const QRegExp &rx, rxs) {
        if (str.contains(rx))
            return true;
    }
    return false;
}

static bool match(const QByteArray &name, const QList<QByteArray> &match)
{
    foreach(const QByteArray &m, match) {
        if (name.contains(m))
            return true;
    }
    return false;
}

static bool match(const QByteArray &, bool)
{
    return true;
}

template <typename T>
static int recurse(const T &t, QByteArray path, const Node *node, uint flags, uint nodeTypes,
                   HandleResult handler, void *userdata)
{
    Q_ASSERT(node);
    int ret = 0;
    if (node->type & nodeTypes) {
        QByteArray full;
        if (flags & VisitThread::MatchSymbolName) {
            full = path + node->symbolName;
            // ### this could maybe be optimized to reuse the same buffer again and again
            if (match(full, t)) {
                handler(node, full, userdata);
                ++ret;
            }
        }
        if (!ret && flags & VisitThread::MatchFileNames) {
            if (match(node->location.path, t)) {
                if (full.isEmpty())
                    full = path + node->symbolName;
                handler(node, full, userdata);
                ++ret;
            }
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
        ret += recurse(t, path, c, flags, nodeTypes, handler, userdata);
    }
    return ret;
}

int VisitThread::lookup(const QList<QByteArray> &patterns, uint flags, uint nodeTypes, HandleResult handler, void *userdata)
{
    QReadLocker lock(&mLock);
    Q_ASSERT(handler);
    if (flags & RegExp) {
        QList<QRegExp> rxs;
        foreach(const QByteArray &pattern, patterns) {
            if (!pattern.isEmpty()) {
                QRegExp rx(pattern);
                if (rx.isValid() && !rx.isEmpty()) {
                    rxs.append(rx);
                }
            }
        }
        if (!rxs.isEmpty())
            return recurse(rxs, QByteArray(), mRoot, flags, nodeTypes, handler, userdata);
    } else if (!patterns.isEmpty()) {
        return recurse(patterns, QByteArray(), mRoot, flags, nodeTypes, handler, userdata);
    }
    return recurse(true, QByteArray(), mRoot, flags, nodeTypes, handler, userdata);
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
