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
        {
            QDebug dbg(&str);
            dbg << cursor;
            // CXCursor ref = clang_getCursorReferenced(cursor);
            // if (isValidCursor(ref))
            //     dbg << ref;
            // CXCursor can = clang_getCanonicalCursor(cursor);
            // if (isValidCursor(can))
            //     dbg << can;
            CXCursor p = clang_getCursorSemanticParent(cursor);
            if (isValidCursor(p))
                dbg << p;
        }

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
                mLongestId = qMax(id.size(), mLongestId);
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
                mLongestId = qMax(id.size(), mLongestId);
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
                   << mNodes.value(id)->symbolName << nodeTypeToName(mNodes.value(id)->type, Normal)
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
            if (kind != CXCursor_MacroExpansion && kind != CXCursor_ClassDecl && kind != CXCursor_StructDecl)
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
        int32_t old = mLongestId;
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
    extern int addedFiles;
    qWarning() << "got here and shit" << addedFiles;
    if (!--addedFiles)
        save(QCoreApplication::instance()->property("output").toByteArray());
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
    return (Int32Length /* type */
            + Int32Length /* Location pos */
            + Int32Length /* parent pos */
            + Int32Length /* nextSibling pos */
            + Int32Length /* firstChild pos */
            + node->symbolName.size() + 1); /*symbolName*/
}

// Format
// 2 * char => Rt
// int32_t => number of Nodes
// int32_t => length of each id
// first id ...
// [int32_t][location padded to length]

static int32_t writeNode(QIODevice *device, Node *node, const QHash<Node*, int32_t> &positions,
                         int entryIdx, int entryLength)
{
    const int32_t nodePosition = positions.value(node, -1);
    if (!node->parent)
        printf("Writing node %s %s at %d\n", nodeTypeToName(node->type, Normal), node->symbolName.constData(), nodePosition);
    Q_ASSERT(nodePosition > 0);
    device->seek(nodePosition);
    writeInt32(device, node->type);
    const int32_t location = (entryIdx == -1 ? 0 : (entryIdx * entryLength) + FirstId + Int32Length);
    writeInt32(device, location);
    writeInt32(device, positions.value(node->parent, 0));
    writeInt32(device, positions.value(node->nextSibling, 0));
    writeInt32(device, positions.value(node->firstChild, 0));
    writeString(device, node->symbolName);
    return nodePosition;
}

static inline int32_t addToDictionary(Node *node, QMap<QByteArray, QSet<qint32> > &map, int32_t pos, int32_t &longestSymbolName)
{
    switch (node->type) {
    case All:
    case Invalid:
    case Root:
    case Reference:
        return 0;
    case Namespace:
    case Class:
    case Struct:
    case MethodDefinition:
    case MethodDeclaration:
    case Variable:
    case Enum:
    case EnumValue:
    case Typedef:
    case MacroDefinition:
        break;
    }

    Node *parent = node->parent;
    QByteArray symbolName = node->symbolName;
    if (node->type == MethodDeclaration || node->type == MethodDefinition) {
        int paren = symbolName.indexOf('(');
        Q_ASSERT(paren != -1);
        symbolName.truncate(paren); // we don't want functions to have to be referenced with full argument list
    }
    map[symbolName].insert(pos);
    Q_ASSERT(!symbolName.contains("("));
    longestSymbolName = qMax(symbolName.size(), longestSymbolName);
    int count = 1;
    while (parent) {
        switch (parent->type) {
            // ### should local variables declared in a function be possible to reference like this:
            // main::a
        case Struct:
        case Class:
        case Namespace:
            ++count;
            symbolName.prepend("::");
            symbolName.prepend(parent->symbolName);
            longestSymbolName = qMax(symbolName.size(), longestSymbolName);
            map[symbolName].insert(pos);
            break;
        default:
            break;
        }
        parent = parent->parent;
    }
    return count;
}

bool VisitThread::save(const QByteArray &path)
{
    // qDebug() << "saving to" << path;
    QFile file(path);
    if (!file.open(QIODevice::WriteOnly)) {
        qWarning() << "Can't open" << path;
        return false;
    }
    
    // ### error checking?
    QMutexLocker lock(&mMutex);
    const int32_t nodeCount = mNodes.size();
    const int entryLength = mLongestId + 1 + Int32Length;
    QByteArray header(rootNodePosition(nodeCount, entryLength), '\0');
    int32_t pos = header.size();
    QHash<Node*, int32_t> positions;
    positions[mRoot] = pos;
    pos += nodeSize(mRoot);
    for (QMap<QByteArray, Node*>::const_iterator it = mNodes.begin(); it != mNodes.end(); ++it) {
        positions[it.value()] = pos;
        pos += nodeSize(it.value());
    }
    file.resize(pos);
    char *out = header.data();
    writeString(out + MagicPos, "Rt");
    writeInt32(out + NodeCountPos, nodeCount);
    // qDebug() << "writing nodeCount to" << NodeCountPos << nodeCount;
    const int idLengthLength = (mLongestId + 1 + Int32Length);
    writeInt32(out + IdLengthPos, idLengthLength);
    // qDebug() << "writing IdLengthPos to" << IdLengthPos << idLengthLength
    //          << readInt32(out + IdLengthPos);


    writeInt32(out + DictionaryPosPos, pos);
    qDebug() << "writing DictionaryPosPos to" << DictionaryPosPos << pos;
    writeNode(&file, mRoot, positions, -1, idLengthLength);
    QMap<QByteArray, int> symbols;
    int entryIdx = 0;
    for (QMap<QByteArray, Node*>::const_iterator it = mNodes.begin(); it != mNodes.end(); ++it) {
        Node *node = it.value();
        const QByteArray &key = it.key();
        const int32_t nodePosition = writeNode(&file, node, positions, entryIdx, entryLength);
        char *s = writeInt32(out + FirstId + (entryIdx * entryLength), nodePosition);
        writeString(s, key);
        ++entryIdx;
    }
    QMap<QByteArray, QSet<int32_t> > dictionary;
    int32_t maxSynonyms = 0;
    int32_t longestSymbolName = 0;
    for (QHash<Node*, int32_t>::const_iterator it = positions.begin(); it != positions.end(); ++it) {
        maxSynonyms = qMax(maxSynonyms, addToDictionary(it.key(), dictionary, it.value(), longestSymbolName));
    }
    writeInt32(out + DictionaryPosPos, pos);
    writeInt32(out + DictionaryCountPos, dictionary.size());
    writeInt32(out + DictionarySymbolNameLengthPos, longestSymbolName);
    writeInt32(out + DictionaryMaxSynonymsPos, maxSynonyms);

    file.seek(0);
    file.write(header);
    file.seek(pos);

    QVarLengthArray<char, 64> nulls(longestSymbolName);
    memset(nulls.data(), '\0', longestSymbolName);
    for (QMap<QByteArray, QSet<int32_t> >::const_iterator it = dictionary.begin(); it != dictionary.end(); ++it) {
        const QSet<int32_t> &locs = it.value();
        foreach(int32_t l, locs)
            writeInt32(&file, l);
        for (int i=locs.size(); i>0; --i) {
            writeInt32(&file, 0); // pad with zeroes for the symbol names that have fewer matches than others
        }
        const QByteArray &symbolName = it.key();
        // qDebug() << "symbolName" << symbolName << locs;
        writeString(&file, symbolName);
        const int diff = longestSymbolName - symbolName.size();
        if (diff < 0)
            qWarning() << longestSymbolName << symbolName;
        if (diff)
            writeString(&file, nulls.constData(), diff);
    }

#if 0
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
#endif
    emit done();
    return true;
}

void VisitThread::onParseError(const Path &path)
{
    qWarning() << "parse error" << path;
    extern int addedFiles;
    if (!--addedFiles)
        save(QCoreApplication::instance()->property("output").toByteArray());
}
