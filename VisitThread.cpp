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
    QElapsedTimer timer;
    timer.start();
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
        Q_ASSERT(!mNodes.contains(it.key()));
        const PendingReference &p = it.value();
        CXCursor ref = clang_getCursorReferenced(p.cursor);
        if (!isValidCursor(ref)) {
            qWarning() << "Can't find referenced cursor for" << p.cursor;
            continue;
        }
        CXCursor def = clang_getCursorDefinition(ref);
        Node *n = new Node(createOrGet(isValidCursor(def) ? def : ref),
                           p.cursor, p.location, it.key());
        const int s = n->size();
        added += s;
        mBytes += s;
        mNodes[it.key()] = n;
    }
    mPendingReferences.clear();
    clang_disposeTranslationUnit(unit);
    qDebug() << mNodes.size() - old << "nodes added for" << path << added << "bytes added total is" << mBytes << timer.elapsed() << "ms";
}
Node * VisitThread::createOrGet(CXCursor cursor)
{
    const CXCursorKind kind = clang_getCursorKind(cursor);
    static const bool verbose = getenv("VERBOSE");
    if (verbose) {
        const Location l(cursor);
        printf("%s at %s:%d:%d\n", kindToString(kind),
               l.path.constData(), l.line, l.column);
    }

    if (clang_isInvalid(kind))
        return mRoot;

    bool delay = false;
    switch (kind) {
    case CXCursor_FirstStmt:
    case CXCursor_FirstExpr:
    case CXCursor_UnexposedDecl:
    case CXCursor_TypedefDecl:
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
    case CXCursor_InvalidFile:
    case CXCursor_NoDeclFound:
    case CXCursor_NotImplemented:
    case CXCursor_InvalidCode:
    case CXCursor_LinkageSpec:
    case CXCursor_BlockExpr:
    case CXCursor_FirstAttr:
    case CXCursor_IBActionAttr:
    case CXCursor_IBOutletAttr:
    case CXCursor_IBOutletCollectionAttr:
    case CXCursor_PreprocessingDirective:
    case CXCursor_MacroDefinition:
    case CXCursor_MacroExpansion:
    case CXCursor_InclusionDirective:
    case CXCursor_TypeAliasDecl:
    case CXCursor_NamespaceAlias:
        if (verbose) {
            const Location l(cursor);
            printf("Ignoring %s at %s:%d:%d\n", kindToString(kind),
                   l.path.constData(), l.line, l.column);
        }
        return createOrGet(clang_getCursorSemanticParent(cursor));
    case CXCursor_TranslationUnit:
        return mRoot;
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
        if (!clang_isCursorDefinition(cursor))
            delay = true;
        break;
    case CXCursor_DeclRefExpr:
    case CXCursor_CallExpr:
    case CXCursor_TypeRef:
    case CXCursor_MemberRefExpr:
        delay = true;
        break;
    case CXCursor_EnumDecl:
    case CXCursor_FieldDecl:
    case CXCursor_EnumConstantDecl:
    case CXCursor_FunctionDecl:
    case CXCursor_VarDecl:
    case CXCursor_ParmDecl:
    case CXCursor_CXXMethod:
    case CXCursor_Namespace:
    case CXCursor_Constructor:
    case CXCursor_Destructor:
    case CXCursor_ConversionFunction:
    case CXCursor_FunctionTemplate:
    case CXCursor_MemberRef:
        // case CXCursor_FirstInvalid:
        // case CXCursor_LastInvalid:
        break;
    }
    const Location location(cursor);
    if (!location.exists())
        return createOrGet(clang_getCursorSemanticParent(cursor));

    const uint hash = qHash(cursor, location);
    if (delay) {
        if (Node *n = mNodes.value(hash))
            return n;
        const PendingReference p = { cursor, location };
        mPendingReferences[hash] = p;
        // CXCursor ref = clang_getCursorReferenced(cursor);
        // bool doPending = isValidCursor(ref) && !mPendingReferences.contains(hash);
        // if (kind == CXCursor_CallExpr && clang_getCursorKind(ref) == CXCursor_CXXMethod) {
        //     doPending = false;
        // } else if (kind == CXCursor_DeclRefExpr && clang_getCursorKind(ref) != CXCursor_CXXMethod) {
        //     doPending = false;
        // }
        // if (doPending) {
        //     qDebug() << "gonna do pending" << cursor << ref;
        //     if (clang_equalCursors(ref, cursor)) {
        //         printf("%s %d: if (clang_equalCursors(ref, cursor)) {\n", __FILE__, __LINE__);
        //         ref = CXCursor();
        //         // For forward declarations of structs ref and cursor is the
        //         // same, probably because at the time we parse this we haven't
        //         // seen the definition of the class, delay initializing ref
        //         // until the end
        //     }

        //     if (!mPendingReferences.contains(hash)) {
        //     mPendingReferences[hash] = p;
        // }
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

static int recurse(Match *match, const Node *node, QByteArray path)
{
    Q_ASSERT(match);
    Q_ASSERT(node);
    int ret = 0;
    if (node->type & match->nodeTypes && match->match(path, node))
        ++ret;

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
    for (Node *c = node->firstChild; c; c = c->nextSibling)
        ret += recurse(match, c, path);
    return ret;
}

int VisitThread::lookup(Match *match)
{
    QReadLocker lock(&mLock);
    Q_ASSERT(match);
    return recurse(match, mRoot, QByteArray());
}

QSet<Path> VisitThread::files() const
{
    QMutexLocker lock(&mMutex);
    return mFiles;
}
