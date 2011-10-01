#include "ClangRunnable.h"
#include "Node.h"
#include <clang-c/Index.h>

Node *ClangRunnable::sRoot = 0;
QMutex ClangRunnable::sTreeMutex(QMutex::Recursive);
QMutex ClangRunnable::sFilesMutex(QMutex::Recursive);
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

struct ComprehensiveTreeUserDataNode {
    QByteArray id;
    Location loc;
    CXCursor cursor, parent;
    Location parentLoc;
    QByteArray parentId;
};
struct ComprehensiveTreeUserData {
    // QList<ComprehensiveTreeUserDataNode> nodes;
    QHash<QByteArray, ComprehensiveTreeUserDataNode> hash;
};

/* There's a reason we don't use clang_equalCursors. It occasionally seems to
 * return 0 when the cursors seemingly are equal
 */

// static bool operator==(const CXCursor &left, const CXCursor &right)
// {
//     if (Location(left).path.isEmpty() && Location(right).path.isEmpty())
//         return true;
//     return (left.kind == right.kind
//             && clang_equalLocations(clang_getCursorLocation(left),
//                                     clang_getCursorLocation(right)));
// }

static CXChildVisitResult dumpTree(CXCursor cursor, CXCursor parent, CXClientData)
{
    for (CXCursor p=clang_getCursorSemanticParent(cursor); isValidCursor(p); p = clang_getCursorSemanticParent(p)) {
        printf("  ");
    }
    QString str;
    {
        QDebug dbg(&str);
        dbg << cursor << (clang_equalCursors(parent, clang_getCursorSemanticParent(cursor)))
            << (clang_equalCursors(parent, clang_getCursorLexicalParent(cursor)))
            << parent << clang_getCursorSemanticParent(cursor)
            << clang_getCursorSemanticParent(clang_getCursorSemanticParent(cursor));
    }
    str.remove("\"");
    printf("%s\n", qPrintable(str));
    return CXChildVisit_Recurse;
}

static CXChildVisitResult buildComprehensiveTree(CXCursor cursor, CXCursor, CXClientData data)
{
    if (Node::nodeTypeFromCursor(cursor) == Invalid)
        return CXChildVisit_Recurse;
    const Location loc(cursor);
    if (loc.path.isEmpty())
        return CXChildVisit_Recurse;
    ComprehensiveTreeUserData *u = reinterpret_cast<ComprehensiveTreeUserData*>(data);
    const QByteArray id = loc.toString();
    if (u->hash.contains(id))
        return CXChildVisit_Recurse;

    CXCursor parent = clang_getCursorSemanticParent(cursor);
    while (clang_getCursorKind(parent) != CXCursor_TranslationUnit && isValidCursor(parent)
           && Node::nodeTypeFromCursor(parent) == Invalid) {
        parent = clang_getCursorSemanticParent(parent);
    }
    const Location parentLoc(parent);
    const QByteArray parentId = parentLoc.toString();
    const ComprehensiveTreeUserDataNode node = { id, loc, cursor, parent, parentLoc, parentId };
    u->hash[id] = node;
    return CXChildVisit_Recurse;
}


ClangRunnable::ClangRunnable(const Path &file, const GccArguments &args,
                             const char *const* clangArgs, int clangArgCount)
    : mFile(file), mArgs(args), mClangArgs(clangArgs), mClangArgCount(clangArgCount)
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
    CXIndex index = clang_createIndex(1, 1);

    QElapsedTimer timer;
    timer.start();
    const time_t lastModified = mFile.lastModified();
    // qDebug() << "parsing file" << mFile << (i == WithPCH ? "with PCH" : "without PCH");
    // for (int i=0; i<mClangArgCount; ++i) {
    //     printf("%s ", mClangArgs[i]);
    // }
    // printf("\n");
    // printf("%s\n", mUnsavedFile->Contents);

    // qDebug() << "calling parse" << mFile << mClangArgs;
    CXTranslationUnit unit = clang_parseTranslationUnit(index, mFile.constData(),
                                                        mClangArgs, mClangArgCount, 0, 0,
                                                        CXTranslationUnit_DetailedPreprocessingRecord); // ### do we need this?
    if (!unit) {
        qWarning("Couldn't parse %s", mFile.constData());

        QByteArray clangLine = "clang";
        if (mArgs.language() == GccArguments::LangCPlusPlus)
            clangLine += "++";
        for (int j=0; j<mClangArgCount; ++j) {
            clangLine += ' ';
            clangLine += mClangArgs[j];
        }
        clangLine += ' ' + mFile;
        qWarning("[%s]", clangLine.constData());
    } else {
        PrecompileData pre;
        clang_getInclusions(unit, precompileHeaders, &pre);
        {
#warning we already have the dependencies in RBuild
            // if (precompile) {
            //     QMutexLocker lock(&sPchMutex);
            //     precompile->add(pre.direct, pre.all);
            // }
            QMutexLocker lock(&sFilesMutex);
            FileData &data = sFiles[mFile];
            data.lastModified = lastModified;
            data.arguments = mArgs;
            foreach(const Path &dependency, pre.all) {
                data.dependencies[dependency] = dependency.lastModified(); // ### raise condition, only checking time after parsing
            }
        }
        processTranslationUnit(mFile, unit);
        printf("Parsed %s\n", mFile.constData());
        clang_disposeTranslationUnit(unit);
    }
    clang_disposeIndex(index);
    emit finished();
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
                if (!symbolName.isEmpty()) {
                    symbolName.prepend("::");
                    symbolName.prepend(parent->symbolName);
                    map[symbolName].insert(locationPos);
                }
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
    QHash<Node*, int32_t> positions;
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
        ++entryIdx;
    }
    QMap<QByteArray, QSet<int32_t> > dictionary;
    for (QHash<Node*, int32_t>::const_iterator it = positions.begin(); it != positions.end(); ++it) {
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

void ClangRunnable::processTranslationUnit(const Path &file, CXTranslationUnit unit)
{
    Q_ASSERT(unit);
    Q_UNUSED(file);
    CXCursor rootCursor = clang_getTranslationUnitCursor(unit);
    ComprehensiveTreeUserData ud;
// #ifndef QT_NO_DEBUG
    const QByteArray dump = qgetenv("RTAGS_DUMP");
    // if (dump == "1" || dump.contains(file.fileName())) {
    //     printf("%s %d: clang_visitChildren(rootCursor, dumpTree, 0);\n", __FILE__, __LINE__);
    //     clang_visitChildren(rootCursor, dumpTree, 0);
    //     fflush(stdout);
    //     sleep(1);
    // }
// #endif
    clang_visitChildren(rootCursor, buildComprehensiveTree, &ud);
    QMutexLocker lock(&sTreeMutex);

    // ### nasty data structures here
    bool doReferences = false;
    forever {

        bool createdNodes = false;
        // qDebug() << iteration++ << Node::sNodes.size() << ud.hash.size() << before;

        QHash<QByteArray, ComprehensiveTreeUserDataNode>::iterator it = ud.hash.begin();
        while (it != ud.hash.end()) {
            ComprehensiveTreeUserDataNode &node = *it;
            if (Node::sNodes.contains(node.id)) {
                // qWarning() << node.cursor << "already exists";
                it = ud.hash.erase(it);
                continue;
            }

            const NodeType type = Node::nodeTypeFromCursor(node.cursor);
            if (type == Reference) {
                if (!doReferences) {
                    ++it;
                    continue;
                }
                Node *referenced = 0;
                if (clang_getCursorKind(node.cursor) == CXCursor_MacroExpansion) {
                    const QByteArray symbolName = eatString(clang_getCursorSpelling(node.cursor));
                    for (Node *n = sRoot->firstChild; n; n = n->nextSibling) {
                        if (n->type == MacroDefinition && n->symbolName == symbolName) {
                            referenced = n;
                            break;
                        }
                    }
                    if (!referenced) {
                        it = ud.hash.erase(it);
                        continue;
                    }
                } else {
                    const CXCursorKind kind = clang_getCursorKind(node.cursor);

                    CXCursor ref = clang_getCursorReferenced(node.cursor);
                    if (clang_equalCursors(ref, node.cursor) && (kind == CXCursor_ClassDecl || kind == CXCursor_StructDecl)) {
                        // ### namespace too?
                        ref = clang_getCursorDefinition(ref);
                    }

                    if (!isValidCursor(ref)) {
                        if (kind != CXCursor_MacroExpansion && kind != CXCursor_ClassDecl && kind != CXCursor_StructDecl)
                            qWarning() << "Can't get valid cursor for" << node.cursor;
                        it = ud.hash.erase(it);
                        continue;
                    }

                    const QByteArray refId = Location(ref).toString();
                    const CXCursorKind refKind = clang_getCursorKind(ref);
                    switch (kind) {
                    case CXCursor_TypeRef:
                        switch (refKind) {
                        case CXCursor_ClassDecl:
                        case CXCursor_StructDecl:
                            if (Node::sNodes.contains(refId))
                                break;
                        case CXCursor_TemplateTypeParameter:
                        case CXCursor_TemplateTemplateParameter:
                        case CXCursor_TemplateRef:
                            it = ud.hash.erase(it);
                            continue;
                        default:
                            break;
                        }
                    default:
                        break;
                    }
                    // if (kind == CXCursor_DeclRefExpr || kind == CXCursor_TypeRef) {
                    //     switch (refKind) {
                    //     case CXCursor_ParmDecl:
                    //     case CXCursor_VarDecl:
                    //     case CXCursor_FieldDecl:
                    //     case CXCursor_CXXMethod:
                    //     case CXCursor_EnumConstantDecl:
                    //     case CXCursor_FunctionDecl:
                    //     case CXCursor_TypeRef:
                    //         break;
                    //     default:
                    //         qDebug() << "throwing out this pending cursor" << node.cursor << ref;
                    //         // fall through
                    //     case CXCursor_NonTypeTemplateParameter:
                    //     case CXCursor_TemplateTypeParameter:
                    //     case CXCursor_TypedefDecl:
                    //         it = ud.hash.erase(it);
                    //         continue;
                    //     }
                    // }
                    referenced = Node::sNodes.value(refId);
                    if (!referenced) {
                        // qWarning() << "Can't find referenced node" << node.cursor << ref << refId;
                        it = ud.hash.erase(it);
                        continue;
                    }
                    if (referenced->type == MethodDefinition) {
                        Node *decl = referenced->methodDeclaration();
                        if (decl)
                            referenced = decl;
                    }
                }
                new Node(referenced, Reference, referenced->symbolName, node.loc, node.id);
                it = ud.hash.erase(it);
                continue;
            } else {
                Node *p = 0;
                if (!node.parentId.isEmpty()) {
                    p = Node::sNodes.value(node.parentId);
                    if (!p) {
                        if (node.id == "/home/anders/dev/rtags/rclient/rclient.c:98:9")
                            qDebug() << "checking shit" << node.cursor;
                        // if (i)
                        //     qWarning() << "Can't find parent" << node.cursor << node.parent;
                        ++it;
                        continue;
                    }
                } else {
                    p = sRoot;
                }

                new Node(p, type, eatString(clang_getCursorDisplayName(node.cursor)),
                         node.loc, node.id);
                createdNodes = true;
                it = ud.hash.erase(it);
            }
        }
        if (doReferences)
            break;
        if (!createdNodes)
            doReferences = true;
    }
}
