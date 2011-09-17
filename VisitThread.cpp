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

struct UserData {
    QList<QPair<CXCursor, CXCursor> > cursors, delayed;
    bool ignoredLast;
    CXCursor lastParent;
};

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

static inline CXChildVisitResult buildList(CXCursor cursor, CXCursor parent, CXClientData data)
{
    UserData *u = reinterpret_cast<UserData*>(data);
    Action a = actionForKind(cursor);
    switch (a) {
    case Ignore:
        u->ignoredLast = true;
        break;
    case Delay:
        if (u->ignoredLast) {
            u->delayed.append(qMakePair(cursor, u->lastParent));
            u->ignoredLast = false;
        } else {
            u->lastParent = parent;
            u->delayed.append(qMakePair(cursor, parent));
        }
        break;
    case Create:
        if (u->ignoredLast) {
            u->cursors.append(qMakePair(cursor, u->lastParent));
            u->ignoredLast = false;
        } else {
            u->lastParent = parent;
            u->cursors.append(qMakePair(cursor, parent));
        }
        break;
    }
    return CXChildVisit_Recurse;
}

struct C {
    C(CXCursor c, C *p = 0)
        : cursor(c), parent(p), firstChild(0), nextSibling(0), lastChild(0)
    {
        if (p) {
            p->append(this);
        }
    }
    CXCursor cursor;
    C *parent, *firstChild, *nextSibling, *lastChild;
    void dump(int indent)
    {
        for (int i=0; i<indent; ++i) {
            printf(" ");
        }

        QString str;
        QDebug(&str) << cursor;
        str.remove("\"");
        printf("%s\n", str.toLocal8Bit().constData());
        for (C *c=firstChild; c; c = c->nextSibling) {
            c->dump(indent + 2);
        }
    }
    void compact()
    {
        Q_ASSERT(!parent || actionForKind(cursor) != Ignore);
        C *c = firstChild;
        C *prev = 0;
        while (c) {
            if (actionForKind(c->cursor) == Ignore) {
                if (!prev) {
                    firstChild = c->nextSibling;
                } else {
                    prev->nextSibling = c->nextSibling;
                }
                if (c->firstChild) {
                    append(c->firstChild);
                }
                C *tmp = c;
                c = c->nextSibling;
                delete tmp;
            } else {
                c->compact();
                prev = c;
                c = c->nextSibling;
            }
        }
    }
    void append(C *c)
    {
        c->parent = this;
        if (lastChild) {
            lastChild->nextSibling = c;
            lastChild = c;
        } else {
            lastChild = firstChild = c;
        }
    }
    void clear()
    {
        for (C *c=firstChild; c; c = c->nextSibling) {
            c->clear();
        }
        delete this;
    }
};
struct ComprehensiveTreeUserData {
    C *root;
    C *last;
    QVector<QPair<CXCursor, C*> > parents;
    CXCursor lastCursor;
};

CXChildVisitResult buildComprehensiveTree(CXCursor cursor, CXCursor parent, CXClientData data)
{
    // qDebug() << cursor << parent;
    ComprehensiveTreeUserData *u = reinterpret_cast<ComprehensiveTreeUserData*>(data);
    C *p = 0;
    bool recursed = false;
    if (!u->root) {
        u->root = new C(parent);
        p = u->root;
        u->parents.append(qMakePair(parent, u->root));
        u->lastCursor = cursor;
    } else {
        Q_ASSERT(u->last);
        if (clang_equalCursors(parent, u->lastCursor)) {
            p = u->last;
            recursed = true;
        } else {
            for (int i=u->parents.size() - 1; i>=0; --i) {
                if (clang_equalCursors(parent, u->parents.at(i).first)) {
                    p = u->parents.at(i).second;
                    u->parents.resize(i + 1);
                    break;
                }
            }
        }
    }
    if (!p) {
        qDebug() << "crashing" << cursor << parent;
        qWarning() << u->root->cursor << clang_equalCursors(u->root->cursor, parent)
                   << clang_equalCursors(parent, parent);
    }
    Q_ASSERT(p);
    u->last = new C(cursor, p);
    if (recursed)
        u->parents.append(qMakePair(parent, p));

    u->lastCursor = cursor;
    return CXChildVisit_Recurse;
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
        ud.root->dump(0);
        // ud.root->compact();
        ud.root->dump(1);
        ud.root->clear();
#if 0
        QElapsedTimer timer;
        timer.start();
        QMutexLocker lock(&mMutex);
        QWriteLocker writeLock(&mLock);
        // const int old = mNodes.size();
        UserData u;
        u.ignoredLast = false;
        u.lastParent = rootCursor;
        // u.cursors += u.delayed; // ### inefficient
        clang_visitChildren(rootCursor, buildList, &u);
        Node *lastRealNode = mRoot;
        QVector<QPair<CXCursor, Node*> > parents;
        CXCursor lastCursor = rootCursor;
        const int count = u.cursors.size();
        for (int i=0; i<count; ++i) {
            const CXCursor &cursor = u.cursors.at(i).first;
            const CXCursor &parent = u.cursors.at(i).second;

            const Action action = actionForKind(cursor);
            if (action != Ignore) {
                qDebug() << cursor << parent << clang_getCursorReferenced(cursor);
            }
            Node *p = 0;
            Q_ASSERT(lastRealNode);
            if (clang_equalCursors(parent, lastCursor)) {
                p = lastRealNode;
                parents.append(qMakePair(parent, p));
            } else {
                for (int j=parents.size() - 1; j>=0; --j) {
                    if (clang_equalCursors(parent, parents.at(j).first)) {
                        p = parents.at(j).second;
                        parents.resize(j + 1);
                        break;
                    }
                }
            }
            if (!p) {
                qWarning() << "no p" << parents << lastCursor;
            }
            lastCursor = cursor;

            Q_ASSERT(p);
            switch (action) {
            case Ignore:
                lastRealNode = p;
                continue;
            case Delay:
                // cursors.append(cursors.at(i));
                // continue;
            case Create:
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
            Node *&node = mNodes[id];
            if (!node) {
                const Node::Type type = Node::typeFromCursor(cursor);
                if (type == Node::Reference) {
                    CXCursor ref = clang_getCursorReferenced(cursor);
                    switch (clang_getCursorKind(ref)) {
                    case CXCursor_StructDecl:
                    case CXCursor_ClassDecl:
                        ref = clang_getCursorDefinition(ref);
                        break;
                    default:
                        break;
                    }
                    const QByteArray parentId = cursorId(ref);
                    p = mNodes.value(parentId);
                    if (!p)
                        qWarning() << cursor << clang_getCursorReferenced(ref) << parentId;
                    Q_ASSERT(p);
                }
                qDebug() << cursor << "inserted" << id;
                lastRealNode = node = new Node(p, type, cursor, location, id);
            }
        }
#endif
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
