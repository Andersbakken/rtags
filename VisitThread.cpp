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

// struct C {
//     C(CXCursor c, C *p = 0) : parent(p), cursor(c) {}
//     C *parent;
//     QList<C*> children;
//     CXCursor cursor;
//     void dump(int indent)
//     {
//         for (int i=0; i<indent; ++i) {
//             printf(" ");
//         }

//         QString str;
//         QDebug(&str) << cursor;
//         str.remove("\"");
//         printf("%s\n", str.toLocal8Bit().constData());
//         foreach(C *child, children) {
//             child->dump(indent + 2);
//         }
//     }

//     void compact()
//     {


//     }
// };

// int foo = 0;
// CXChildVisitResult count(CXCursor cursor, CXCursor parent, CXClientData data)
// {
//     // qDebug() << cursor << parent;
//     UserData *u = reinterpret_cast<UserData*>(data);
//     C *p = 0;
//     bool recursed = false;
//     if (!u->root) {
//         u->root = new C(parent);
//         p = u->root;
//         u->parents.append(qMakePair(parent, u->root));
//     } else {
//         Q_ASSERT(u->last);
//         if (clang_equalCursors(parent, u->lastCursor)) {
//             p = u->last;
//             recursed = true;
//         } else {
//             for (int i=u->parents.size() - 1; i>=0; --i) {
//                 if (clang_equalCursors(parent, u->parents.at(i).first)) {
//                     p = u->parents.at(i).second;
//                     u->parents.resize(i + 1);
//                     break;
//                 }
//             }
//             if (!p) {
//                 qDebug() << "fucked" << parent;
//                 for (int i=u->parents.size() - 1; i>=0; --i) {
//                     qDebug() << "comparing" << i << u->parents.size()
//                              << u->parents.at(i).first;
//                     // if (clang_equalCursors(parent, u->parents.at(i).first)) {
//                     //     p = u->parents.at(i).second;
//                     //     u->parents.resize(i + 1);
//                     //     break;
//                     // }
//                 }


//             }
//         }
//     }
//     ++foo;
//     if (!p) {
//         qDebug() << "crashing" << foo << cursor << parent;
//         qWarning() << u->root->cursor << clang_equalCursors(u->root->cursor, parent)
//                    << clang_equalCursors(parent, parent);
//     }
//     Q_ASSERT(p);
//     if (clang_getCursorKind(cursor) == CXCursor_FirstExpr
//         || clang_getCursorKind(cursor) == CXCursor_FirstStmt) {
//         u->last = p;
//     } else {
//         u->last = new C(cursor, p);
//         p->children.append(u->last);
//     }
//     if (recursed)
//         u->parents.append(qMakePair(parent, p));

//     u->lastCursor = cursor;
//     return CXChildVisit_Recurse;
// }

struct PendingReference {
    CXCursor cursor;
    Location location;
};

struct UserData {
    UserData(Node *&root, QHash<QByteArray, Node*> &ref)
        : rootRef(root), lastRealNode(root), nodesRef(ref),
          lastCursor(clang_getNullCursor())
    {}

    Node *&rootRef;
    Node *lastRealNode;
    QVector<QPair<CXCursor, Node*> > parents;
    QHash<QByteArray, Node*> &nodesRef;
    CXCursor lastCursor;
    QHash<QByteArray, PendingReference> pendingReferences;
};

void VisitThread::onFileParsed(const Path &path, void *u)
{
    CXTranslationUnit unit = reinterpret_cast<CXTranslationUnit>(u);
    if (!mQuitting) {
        QElapsedTimer timer;
        timer.start();
        QMutexLocker lock(&mMutex);
        QWriteLocker writeLock(&mLock);
        // const int old = mNodes.size();
        CXCursor cursor = clang_getTranslationUnitCursor(unit);
        // int c = 0;
        // UserData u;
        // u.last = u.root = 0;
        // u.lastCursor = clang_getNullCursor();
        // clang_visitChildren(cursor, count, &u);
        // u.root->dump(0);
        // qDebug() << foo;
        // exit(0);

        UserData userData(mRoot, mNodes);
        userData.lastCursor = cursor;
        clang_visitChildren(cursor, buildTree, &userData);
        foreach(CXCursor c, all) {
            qWarning() << c << clang_getCursorReferenced(c);
        }
        // qDebug() << "We have" << c << "cursors"
        //          << "tot" << tot;
        //     for (QHash<QByteArray, PendingReference>::const_iterator it = mPendingReferences.begin();
        //          it != mPendingReferences.end(); ++it) {
        //         if (mNodes.contains(it.key())) {
        //             qWarning() << "somehow this node now exists" << it.value().cursor;
        //             continue;
        //         }
        //         Q_ASSERT(!mNodes.contains(it.key()));
        //         const PendingReference &p = it.value();
        //         CXCursor ref = clang_getCursorReferenced(p.cursor);
        //         if (!isValidCursor(ref)) {
        //             // qWarning() << "Can't find referenced cursor for" << p.cursor;
        //             continue;
        //         }
        //         const CXCursorKind kind = clang_getCursorKind(p.cursor);
        //         const CXCursorKind refKind = clang_getCursorKind(ref);
        //         if (kind == CXCursor_CallExpr && refKind == CXCursor_CXXMethod) {
        //             continue; // these have the wrong Location, we get the right one from CXCursor_DeclRefExpr
        //         } else if (kind == CXCursor_DeclRefExpr) {
        //             switch (refKind) {
        //             case CXCursor_ParmDecl:
        //             case CXCursor_VarDecl:
        //             case CXCursor_CXXMethod:
        //             case CXCursor_EnumConstantDecl:
        //                 break;
        //             default:
        //                 // qDebug() << "throwing out this pending one" << p.cursor << ref;
        //                 continue;
        //             }
        //         }

        //         CXCursor def = clang_getCursorDefinition(ref);
        //         Node *n = new Node(createOrGet(isValidCursor(def) ? def : ref),
        //                            p.cursor, p.location, it.key());
        //         const int s = n->size();
        //         added += s;
        //         mBytes += s;
        //         mNodes[it.key()] = n;
        //     }
        //     qDebug() << mNodes.size() - old << "nodes added for" << path << added << "bytes added total is"
        //              << mBytes << timer.elapsed() << "ms";
    }
    clang_disposeTranslationUnit(unit);
}
// Node * VisitThread::createOrGet(CXCursor cursor)
// {
//     ++tot;
//     const CXCursorKind kind = clang_getCursorKind(cursor);
//     static const bool verbose = getenv("VERBOSE");
//     if (verbose) {
//         const Location l(cursor);
//         printf("%s at %s:%d:%d\n", kindToString(kind),
//                l.path.constData(), l.line, l.column);
//     }

//     if (clang_isInvalid(kind))
//         return mRoot;

//     bool delay = false;
//     switch (kind) {
//     case CXCursor_FirstStmt:
//     case CXCursor_FirstExpr:
//     case CXCursor_UnexposedDecl:
//     case CXCursor_TypedefDecl:
//     case CXCursor_UsingDirective:
//     case CXCursor_NamespaceRef:
//     case CXCursor_TemplateTypeParameter:
//     case CXCursor_OverloadedDeclRef:
//     case CXCursor_CXXBaseSpecifier:
//     case CXCursor_ClassTemplate:
//     case CXCursor_NonTypeTemplateParameter:
//     case CXCursor_TemplateRef:
//     case CXCursor_UnionDecl:
//     case CXCursor_ClassTemplatePartialSpecialization:
//     case CXCursor_LabelStmt:
//     case CXCursor_LabelRef:
//     case CXCursor_UsingDeclaration:
//     case CXCursor_TemplateTemplateParameter:
//     case CXCursor_ObjCSuperClassRef:
//     case CXCursor_ObjCInterfaceDecl:
//     case CXCursor_ObjCCategoryDecl:
//     case CXCursor_ObjCProtocolDecl:
//     case CXCursor_ObjCPropertyDecl:
//     case CXCursor_ObjCIvarDecl:
//     case CXCursor_ObjCInstanceMethodDecl:
//     case CXCursor_ObjCClassMethodDecl:
//     case CXCursor_ObjCImplementationDecl:
//     case CXCursor_ObjCCategoryImplDecl:
//     case CXCursor_ObjCSynthesizeDecl:
//     case CXCursor_ObjCDynamicDecl:
//     case CXCursor_ObjCProtocolRef:
//     case CXCursor_ObjCClassRef:
//     case CXCursor_ObjCMessageExpr:
//     case CXCursor_InvalidFile:
//     case CXCursor_NoDeclFound:
//     case CXCursor_NotImplemented:
//     case CXCursor_InvalidCode:
//     case CXCursor_LinkageSpec:
//     case CXCursor_BlockExpr:
//     case CXCursor_FirstAttr:
//     case CXCursor_IBActionAttr:
//     case CXCursor_IBOutletAttr:
//     case CXCursor_IBOutletCollectionAttr:
//     case CXCursor_PreprocessingDirective:
//     case CXCursor_MacroDefinition:
//     case CXCursor_MacroExpansion:
//     case CXCursor_InclusionDirective:
//     case CXCursor_TypeAliasDecl:
//     case CXCursor_NamespaceAlias:
//     case CXCursor_CXXFinalAttr:
//     case CXCursor_CXXOverrideAttr:
//         if (verbose) {
//             const Location l(cursor);
//             printf("Ignoring %s at %s:%d:%d\n", kindToString(kind),
//                    l.path.constData(), l.line, l.column);
//         }
//         return createOrGet(clang_getCursorSemanticParent(cursor));
//     case CXCursor_TranslationUnit:
//         return mRoot;
//     case CXCursor_ClassDecl:
//     case CXCursor_StructDecl:
//         if (!clang_isCursorDefinition(cursor))
//             delay = true;
//         break;
//     case CXCursor_DeclRefExpr:
//     case CXCursor_CallExpr:
//     case CXCursor_TypeRef:
//     case CXCursor_MemberRefExpr:
//         delay = true;
//         break;
//     case CXCursor_EnumDecl:
//     case CXCursor_FieldDecl:
//     case CXCursor_EnumConstantDecl:
//     case CXCursor_FunctionDecl:
//     case CXCursor_VarDecl:
//     case CXCursor_ParmDecl:
//     case CXCursor_CXXMethod:
//     case CXCursor_Namespace:
//     case CXCursor_Constructor:
//     case CXCursor_Destructor:
//     case CXCursor_ConversionFunction:
//     case CXCursor_FunctionTemplate:
//     case CXCursor_MemberRef:
//         // case CXCursor_FirstInvalid:
//         // case CXCursor_LastInvalid:
//         break;
//     }
//     const Location location(cursor);
//     if (!location.exists())
//         return createOrGet(clang_getCursorSemanticParent(cursor));

//     const uint hash = qHash(cursor, location);
//     if (delay) {
//         if (Node *n = mNodes.value(hash))
//             return n;
//         const PendingReference p = { cursor, location };
//         mPendingReferences[hash] = p;
//         return createOrGet(clang_getCursorSemanticParent(cursor));
//     }

//     Node *&node = mNodes[hash];
//     if (!node) {
//         node = new Node(createOrGet(clang_getCursorSemanticParent(cursor)), cursor, location, hash);
//         mPendingReferences.remove(hash);
//         Q_ASSERT(node->parent);
//     }
//     return node;
// }

enum Action {
    Ignore,
    Delay,
    Create
};
static inline Action actionForKind(const CXCursor &cursor) // ### pass kind, not cursor
{
    switch (clang_getCursorKind(cursor)) {
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
    case CXCursor_CXXFinalAttr:
    case CXCursor_CXXOverrideAttr:
        // if (verbose) {
        //     const Location l(cursor);
        //     printf("Ignoring %s at %s:%d:%d\n", kindToString(kind),
        //            l.path.constData(), l.line, l.column);
        // }
        return Ignore;
    case CXCursor_TranslationUnit:
        Q_ASSERT(0);
        break;
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
        if (!clang_isCursorDefinition(cursor))
            return Delay;
        break;
    case CXCursor_DeclRefExpr:
    case CXCursor_CallExpr:
    case CXCursor_TypeRef:
    case CXCursor_MemberRefExpr:
        return Delay;
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
    return Create;
}

CXChildVisitResult VisitThread::buildTree(CXCursor cursor, CXCursor parent, CXClientData data)
{
    UserData *u = reinterpret_cast<UserData*>(data);
    const Action action = actionForKind(cursor);
    if (action != Ignore) {
        qDebug() << cursor << parent << clang_getCursorReferenced(cursor);
        all << cursor;
    }
    Node *p = 0;
    Q_ASSERT(u->lastRealNode);
    if (clang_equalCursors(parent, u->lastCursor)) {
        p = u->lastRealNode;
        u->parents.append(qMakePair(parent, p));
    } else {
        for (int i=u->parents.size() - 1; i>=0; --i) {
            if (clang_equalCursors(parent, u->parents.at(i).first)) {
                p = u->parents.at(i).second;
                u->parents.resize(i + 1);
                break;
            }
        }
    }
    u->lastCursor = cursor;
    Q_ASSERT(p);
    switch (action) {
    case Ignore:
        u->lastRealNode = p;
        return CXChildVisit_Recurse;
    case Create:
    case Delay:
        break;
    }
    const Location location(cursor);
    if (!location.exists()) {
        qWarning() << "No location for this node" << cursor;
    }
    // CXCursorKind kind = clang_getCursorKind(kind);
    // if (kind == CXCursor_CallExpr && refKind == CXCursor_CXXMethod) {

    // if (clang_getCursorKind
    Q_ASSERT(location.exists());
    const QByteArray id = cursorId(cursor, location); // ### hash in GccArguments used
    Node *&node = u->nodesRef[id];
    if (!node) {
        u->lastRealNode = node = new Node(p, cursor, location, id);
    } else {
        return CXChildVisit_Continue;
    }

    return CXChildVisit_Recurse;
   
    // VisitThread *visitThread = reinterpret_cast<VisitThread*>(data);

    // bool delay = false;
    // switch (kind) {
    // case CXCursor_FirstStmt:
    // case CXCursor_FirstExpr:
    // case CXCursor_UnexposedDecl:
    // case CXCursor_TypedefDecl:
    // case CXCursor_UsingDirective:
    // case CXCursor_NamespaceRef:
    // case CXCursor_TemplateTypeParameter:
    // case CXCursor_OverloadedDeclRef:
    // case CXCursor_CXXBaseSpecifier:
    // case CXCursor_ClassTemplate:
    // case CXCursor_NonTypeTemplateParameter:
    // case CXCursor_TemplateRef:
    // case CXCursor_UnionDecl:
    // case CXCursor_ClassTemplatePartialSpecialization:
    // case CXCursor_LabelStmt:
    // case CXCursor_LabelRef:
    // case CXCursor_UsingDeclaration:
    // case CXCursor_TemplateTemplateParameter:
    // case CXCursor_ObjCSuperClassRef:
    // case CXCursor_ObjCInterfaceDecl:
    // case CXCursor_ObjCCategoryDecl:
    // case CXCursor_ObjCProtocolDecl:
    // case CXCursor_ObjCPropertyDecl:
    // case CXCursor_ObjCIvarDecl:
    // case CXCursor_ObjCInstanceMethodDecl:
    // case CXCursor_ObjCClassMethodDecl:
    // case CXCursor_ObjCImplementationDecl:
    // case CXCursor_ObjCCategoryImplDecl:
    // case CXCursor_ObjCSynthesizeDecl:
    // case CXCursor_ObjCDynamicDecl:
    // case CXCursor_ObjCProtocolRef:
    // case CXCursor_ObjCClassRef:
    // case CXCursor_ObjCMessageExpr:
    // case CXCursor_InvalidFile:
    // case CXCursor_NoDeclFound:
    // case CXCursor_NotImplemented:
    // case CXCursor_InvalidCode:
    // case CXCursor_LinkageSpec:
    // case CXCursor_BlockExpr:
    // case CXCursor_FirstAttr:
    // case CXCursor_IBActionAttr:
    // case CXCursor_IBOutletAttr:
    // case CXCursor_IBOutletCollectionAttr:
    // case CXCursor_PreprocessingDirective:
    // case CXCursor_MacroDefinition:
    // case CXCursor_MacroExpansion:
    // case CXCursor_InclusionDirective:
    // case CXCursor_TypeAliasDecl:
    // case CXCursor_NamespaceAlias:
    // case CXCursor_CXXFinalAttr:
    // case CXCursor_CXXOverrideAttr:
    //     if (verbose) {
    //         const Location l(cursor);
    //         printf("Ignoring %s at %s:%d:%d\n", kindToString(kind),
    //                l.path.constData(), l.line, l.column);
    //     }
    //     return createOrGet(clang_getCursorSemanticParent(cursor));
    // case CXCursor_TranslationUnit:
    //     return mRoot;
    // case CXCursor_ClassDecl:
    // case CXCursor_StructDecl:
    //     if (!clang_isCursorDefinition(cursor))
    //         delay = true;
    //     break;
    // case CXCursor_DeclRefExpr:
    // case CXCursor_CallExpr:
    // case CXCursor_TypeRef:
    // case CXCursor_MemberRefExpr:
    //     delay = true;
    //     break;
    // case CXCursor_EnumDecl:
    // case CXCursor_FieldDecl:
    // case CXCursor_EnumConstantDecl:
    // case CXCursor_FunctionDecl:
    // case CXCursor_VarDecl:
    // case CXCursor_ParmDecl:
    // case CXCursor_CXXMethod:
    // case CXCursor_Namespace:
    // case CXCursor_Constructor:
    // case CXCursor_Destructor:
    // case CXCursor_ConversionFunction:
    // case CXCursor_FunctionTemplate:
    // case CXCursor_MemberRef:
    //     // case CXCursor_FirstInvalid:
    //     // case CXCursor_LastInvalid:
    //     break;
    // }
    // const Location location(cursor);
    // if (!location.exists())
    //     return createOrGet(clang_getCursorSemanticParent(cursor));

    // const uint hash = qHash(cursor, location);
    // if (delay) {
    //     if (Node *n = mNodes.value(hash))
    //         return n;
    //     const PendingReference p = { cursor, location };
    //     mPendingReferences[hash] = p;
    //     return createOrGet(clang_getCursorSemanticParent(cursor));
    // }

    // Node *&node = mNodes[hash];
    // if (!node) {
    //     node = new Node(createOrGet(clang_getCursorSemanticParent(cursor)), cursor, location, hash);
    //     mPendingReferences.remove(hash);
    //     Q_ASSERT(node->parent);
    // }
    // return node;
    
    return CXChildVisit_Recurse;
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
    QWriteLocker writeLock(&mLock);
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
    QReadLocker lock(&mLock);
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
    QReadLocker lock(&mLock);
    Q_ASSERT(match);
    recurse(match, mRoot, QByteArray());
}
