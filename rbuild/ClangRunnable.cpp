#include "ClangRunnable.h"
#include "Node.h"
#include "PreCompile.h"
#include <clang-c/Index.h>

static const bool disablePch = getenv("RTAGS_NO_PCH");
Node *ClangRunnable::sRoot = 0;
QMutex ClangRunnable::sPchMutex(QMutex::Recursive);
QMutex ClangRunnable::sTreeMutex(QMutex::Recursive);
QHash<Path, ClangRunnable::FileData> ClangRunnable::sFiles;

struct PrecompileData {
    QList<Path> direct, all;
};

static inline void precompileHeaders(CXFile included_file, CXSourceLocation*,
                                     unsigned include_len, CXClientData client_data)
{
    if (!include_len)
        return;

    CXString filename = clang_getFileName(included_file);

    PrecompileData* data = reinterpret_cast<PrecompileData*>(client_data);
    Path rfn = Path::resolved(clang_getCString(filename));
    if (include_len == 1)
        data->direct.append(rfn);
    data->all.append(rfn);
    clang_disposeString(filename);
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

static CXChildVisitResult buildComprehensiveTree(CXCursor cursor, CXCursor parent, CXClientData data)
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

ClangRunnable::ClangRunnable(const Path &file, const GccArguments &args)
    : mFile(file), mArgs(args)
{
    setAutoDelete(true);
}

void ClangRunnable::init()
{
    sRoot = new Node;
}

void ClangRunnable::cleanup()
{
    delete sRoot;
    sRoot = 0;
}

void ClangRunnable::run()
{
    CXIndex index = clang_createIndex(1, 0);

    QElapsedTimer timer;
    timer.start();
    QVector<const char*> args;
    const QList<QByteArray> compilerOptions = mArgs.includePaths() + mArgs.arguments("-D");
    const int compilerOptionsCount = compilerOptions.count();

    Path pchfile;
    int argCount = compilerOptions.size();
    PreCompile *precompile = 0;
    if (!disablePch && mArgs.language() == GccArguments::LangCPlusPlus) {
        QMutexLocker lock(&sPchMutex);
        precompile = PreCompile::get(compilerOptions);
        pchfile = precompile->filename();
        if (pchfile.isFile())
            argCount += 2;
    }
    args.resize(argCount);
    for (int a=0; a<compilerOptionsCount; ++a) {
        args[a] = compilerOptions.at(a).constData();
    }
    if (argCount > compilerOptionsCount) {
        Q_ASSERT(argCount - compilerOptionsCount == 2);
        Q_ASSERT(!pchfile.isEmpty());
        args[compilerOptionsCount] = "-pch";
        args[compilerOptionsCount + 1] = pchfile.constData();
    }

    const time_t lastModified = mFile.lastModified();
    // qDebug() << "parsing file" << mFile << (i == WithPCH ? "with PCH" : "without PCH");
    Q_ASSERT(!args.contains(0));
    // for (int i=0; i<argCount; ++i) {
    //     printf("%d [%s]\n", i, args.constData()[i]);
    // }

    // qDebug() << "calling parse" << mFile << args;
    CXTranslationUnit unit = clang_parseTranslationUnit(index, mFile.constData(),
                                                        args.constData(), argCount, 0, 0,
                                                        // CXTranslationUnit_NestedMacroExpansions
                                                        CXTranslationUnit_DetailedPreprocessingRecord); // ### do we need this?
    if (!unit) {
        qWarning("Couldn't parse %s", mFile.constData());
        QByteArray clangLine = "clang";
        if (mArgs.language() == GccArguments::LangCPlusPlus)
            clangLine += "++";
        for (int j=0; j<argCount; ++j) {
            clangLine += ' ';
            clangLine += args.at(j);
        }
        clangLine += ' ' + mFile;
        qWarning("[%s]", clangLine.constData());
    } else {
        PrecompileData pre;
        clang_getInclusions(unit, precompileHeaders, &pre);
        {
            QMutexLocker lock(&sPchMutex);
            if (precompile)
                precompile->add(pre.direct, pre.all);
            FileData &data = sFiles[mFile];
            data.lastModified = lastModified;
            data.arguments = mArgs;
            foreach(const Path &dependency, pre.all) {
                data.dependencies[dependency] = dependency.lastModified(); // ### raise condition, only checking time after parsing
            }
        }
        CXCursor rootCursor = clang_getTranslationUnitCursor(unit);
        ComprehensiveTreeUserData ud;
        ud.last = ud.root = 0;
        ud.lastCursor = clang_getNullCursor();
        clang_visitChildren(rootCursor, buildComprehensiveTree, &ud);
#ifndef QT_NO_DEBUG
        const QByteArray dump = qgetenv("RTAGS_DUMP");
        if (ud.root && (dump == "1" || dump.contains(mFile.fileName()))) {
            ud.root->dump(0);
            printf("Tree done\n");
            fflush(stdout);
            sleep(1);
        }
#endif
        QHash<QByteArray, PendingReference> references;
        if (ud.root) {
            int old;
            {
                QMutexLocker lock(&sTreeMutex);
                old = Node::sNodes.size();
                buildTree(sRoot, ud.root, references);
                for (QHash<QByteArray, PendingReference>::const_iterator it = references.begin(); it != references.end(); ++it) {
                    const PendingReference &p = it.value();
                    addReference(p.node, it.key(), p.location);
                }
            }
            delete ud.root;
            qDebug() << "added" << (Node::sNodes.size() - old) << "nodes for" << mFile << ". Total" << Node::sNodes.size()
                     << timer.elapsed() << "ms" << (argCount != compilerOptionsCount ? "with PCH" : "without PCH");
        }
        clang_disposeTranslationUnit(unit);
    }
    clang_disposeIndex(index);
    emit finished();
}

void ClangRunnable::buildTree(Node *parent, CursorNode *c, QHash<QByteArray, PendingReference> &references)
{
    Q_ASSERT(c);
    if (clang_getCursorKind(c->cursor) == CXCursor_MacroExpansion) {
        const Location loc(c->cursor);
        Q_ASSERT(!loc.isNull());
        const QByteArray id = loc.toString();
        if (Node::sNodes.contains(id))
            return;
        const QByteArray symbolName = eatString(clang_getCursorSpelling(c->cursor));
        for (CursorNode *cn = c->parent->firstChild; cn; cn = cn->nextSibling) {
            if (clang_getCursorKind(cn->cursor) == CXCursor_MacroDefinition
                && symbolName == eatString(clang_getCursorSpelling(cn->cursor))) {
                const QByteArray macroDefinitionId = Location(cn->cursor).toString();
                Node *parent = Node::sNodes.value(macroDefinitionId);
                Q_ASSERT(parent);
                new Node(parent, Reference, parent->symbolName, loc, id);
                return;
            }
        }
    }

    const NodeType type = Node::nodeTypeFromCursor(c->cursor);
    if (type == Reference) {
        const Location loc(c->cursor);
        if (loc.exists()) {
            const QByteArray id = loc.toString();
            if (!Node::sNodes.contains(id)) {
                const PendingReference r = { c, loc };
                references[id] = r;
            }
        }
    } else {
        if (c->parent && type != Invalid) {
            const Location loc(c->cursor);
            if (loc.exists()) {
                const QByteArray id = loc.toString();
                if (Node::sNodes.contains(id))
                    return;
                // ### may not need to do this for all types of nodes
                CXCursor realParent = clang_getCursorSemanticParent(c->cursor);
                if (isValidCursor(realParent) && !clang_equalCursors(realParent, c->parent->cursor)) {
                    const QByteArray parentId = Location(realParent).toString();
                    parent = Node::sNodes.value(parentId, parent);
                }

                parent = new Node(parent, type,
                                  (type == Reference
                                   ? parent->symbolName
                                   : eatString(clang_getCursorDisplayName(c->cursor))),
                                  loc, id);
            }
        }
        for (CursorNode *child=c->firstChild; child; child = child->nextSibling) {
            buildTree(parent, child, references);
        }
    }
}

void ClangRunnable::addReference(CursorNode *c, const QByteArray &id, const Location &loc)
{
    if (Node::sNodes.contains(id)) {
        qWarning() << "Turns out" << c->cursor << "already exists"
                   << Node::sNodes.value(id)->symbolName << nodeTypeToName(Node::sNodes.value(id)->type, Normal)
                   << Node::sNodes.value(id)->location;
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
        Node *refNode = Node::sNodes.value(refId);
        if (!refNode) {
            // qWarning() << "Can't find referenced node" << c->cursor << ref << refId;
            return;
        }
        if (refNode->type == MethodDefinition) {
            Node *decl = refNode->methodDeclaration();
            if (decl)
                refNode = decl;
        }
        Q_ASSERT(!Node::sNodes.contains(id));
        new Node(refNode, Reference, refNode->symbolName, loc, id);
        Q_ASSERT(Node::sNodes.contains(id));
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
    // if (node->parent)
    //     printf("Writing node %s %s at %d\n", nodeTypeToName(node->type, Normal), node->symbolName.constData(), nodePosition);
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

static inline void addToDictionary(Node *node, QMap<QByteArray, QSet<qint32> > &map, int32_t locationPos)
{
    switch (node->type) {
    case All:
    case Invalid:
    case Root:
    case Reference:
        return;
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
    if (!node->symbolName.isEmpty()) {
        Node *parent = node->parent;
        QByteArray symbolName = node->symbolName;
        if (node->type == MethodDeclaration || node->type == MethodDefinition) {
            int paren = symbolName.indexOf('(');
            Q_ASSERT(paren != -1);
            symbolName.truncate(paren); // we don't want functions to have to be referenced with full argument list
        }
        map[symbolName].insert(locationPos);
        Q_ASSERT(!symbolName.contains("("));
        while (parent) {
            switch (parent->type) {
                // ### should local variables declared in a function be possible to reference like this:
                // main::a
            case Struct:
            case Class:
            case Namespace:
                symbolName.prepend("::");
                symbolName.prepend(parent->symbolName);
                map[symbolName].insert(locationPos);
                break;
            default:
                break;
            }
            parent = parent->parent;
        }
    }
}

bool ClangRunnable::save(const QByteArray &path)
{
    // qDebug() << "saving to" << path;
    QFile file(path);
    if (!file.open(QIODevice::WriteOnly)) {
        qWarning() << "Can't open" << path;
        return false;
    }

    // ### error checking?
    QMutexLocker lock(&sTreeMutex);
    const int32_t nodeCount = Node::sNodes.size();
    const int entryLength = Node::sLongestId + 1 + Int32Length;
    QByteArray header(rootNodePosition(nodeCount, entryLength), '\0');
    int32_t pos = header.size();
    QHash<Node*, int32_t> positions, locationsForNode;
    positions[sRoot] = pos;
    pos += nodeSize(sRoot);
    for (QMap<QByteArray, Node*>::const_iterator it = Node::sNodes.begin(); it != Node::sNodes.end(); ++it) {
        positions[it.value()] = pos;
        pos += nodeSize(it.value());
    }
    file.resize(pos);
    // qDebug() << "file is" << pos;
    char *out = header.data();
    writeString(out + MagicPos, "Rt");
    writeInt32(out + NodeCountPos, nodeCount);
    // qDebug() << "writing nodeCount to" << NodeCountPos << nodeCount;
    const int idLengthLength = (Node::sLongestId + 1 + Int32Length);
    writeInt32(out + IdLengthPos, idLengthLength);
    // qDebug() << "writing IdLengthPos to" << IdLengthPos << idLengthLength
    //          << readInt32(out + IdLengthPos);


    writeInt32(out + DictionaryPosPos, pos);
    // qDebug() << "writing DictionaryPosPos" << pos;
    writeNode(&file, sRoot, positions, -1, idLengthLength);
    QMap<QByteArray, int> symbols;
    int entryIdx = 0;
    for (QMap<QByteArray, Node*>::const_iterator it = Node::sNodes.begin(); it != Node::sNodes.end(); ++it) {
        Node *node = it.value();
        const QByteArray &key = it.key();
        const int32_t nodePosition = writeNode(&file, node, positions, entryIdx, entryLength);
        char *s = writeInt32(out + FirstId + (entryIdx * entryLength), nodePosition);
        writeString(s, key);
        const int32_t location = ((entryIdx * entryLength) + FirstId + Int32Length);
        locationsForNode[node] = location;
        ++entryIdx;
    }
    QMap<QByteArray, QSet<int32_t> > dictionary;
    for (QHash<Node*, int32_t>::const_iterator it = locationsForNode.begin(); it != locationsForNode.end(); ++it) {
        addToDictionary(it.key(), dictionary, it.value());
    }

    writeInt32(out + DictionaryPosPos, pos);
    writeInt32(out + DictionaryCountPos, dictionary.size());

    file.seek(pos);

    for (QMap<QByteArray, QSet<int32_t> >::const_iterator it = dictionary.begin(); it != dictionary.end(); ++it) {
        // qDebug() << file.pos() << it.key() << it.value();
        writeString(&file, it.key());
        foreach(int32_t l, it.value()) {
            // qDebug() << "writing location" << l << "at" << file.pos();
            writeInt32(&file, l);
        }
        writeInt32(&file, 0);
    }
    pos = file.pos();
    writeInt32(out + FileDataPosPos, pos);
    file.seek(0);
    file.write(header);
    file.seek(pos);
    {
        QDataStream ds(&file);
        qDebug() << "saved" << sFiles.size() << "files at" << file.pos()
                 << Node::sNodes.size() << "nodes";
        ds << sFiles;
    }
    // for (QMap<Path, FileData>::const_iterator it = sFiles.begin(); it != sFiles.end(); ++it) {
    //     writeString(&file, it.key());
    //     const FileData &data = it.value();
    //     writeInt64(&file, data.lastModified);
    //     QDataStream ds(&file);
    //     ds << data.arguments;
    //     const QHash<Path, time_t> &headers = data.dependencies;
    //     writeInt32(&file, headers.size());
    //     for (QHash<Path, time_t>::const_iterator it = headers.begin(); it != headers.end(); ++it) {
    //         writeString(&file, it.key());
    //         writeInt64(&file, it.value());
    //     }
    // }

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
    return true;
}

QDataStream &operator<<(QDataStream &ds, const ClangRunnable::FileData &fd)
{
    ds << fd.arguments << fd.lastModified << fd.dependencies;
    return ds;
}

QDataStream &operator>>(QDataStream &ds, ClangRunnable::FileData &fd)
{
    ds >> fd.arguments >> fd.lastModified >> fd.dependencies;
    return ds;
}

void ClangRunnable::initTree(const MMapData *data, const QSet<Path> &modifiedPaths)
{
    QMutexLocker lock(&sTreeMutex);
    Q_ASSERT(sRoot);
    // qDebug() << modifiedPaths;
    const NodeData nodeData = readNodeData(data->memory + data->rootNodePosition);
    initTree(data, modifiedPaths, sRoot, nodeData);
}


void ClangRunnable::initTree(const MMapData *data, const QSet<Path> &modifiedPaths,
                             Node *parent, const NodeData &nodeData)
{
    int child = nodeData.firstChild;
    while (child) {
        const NodeData c = readNodeData(data->memory + child);
        const Location loc(data->memory + c.location);
        if (!modifiedPaths.contains(loc.path)) {
            const QByteArray id = loc.toString();
            Node *node = Node::sNodes.value(id);
            if (!node)
                node = new Node(parent, static_cast<NodeType>(c.type), QByteArray(c.symbolName), loc, id);
            initTree(data, modifiedPaths, node, c);
        }
        child = c.nextSibling;
    }
}
