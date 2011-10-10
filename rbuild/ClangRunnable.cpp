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
    CXCursor cursor, parent, hackParent;
    Location parentLoc;
    QByteArray parentId;
};

static CXChildVisitResult dumpTree(CXCursor cursor, CXCursor, CXClientData)
{
    QString str;
    {
        QDebug dbg(&str);
        dbg << cursor << clang_getCursorReferenced(cursor);// << clang_getCursorSemanticParent(cursor)
        // << parent << clang_getCursorLexicalParent(cursor);
    }
    str.remove("\"");
    qDebug("%s", qPrintable(str));
    return CXChildVisit_Recurse;
}

static CXChildVisitResult buildComprehensiveTree(CXCursor cursor, CXCursor hackParent, CXClientData data)
{
    if (Node::nodeTypeFromCursor(cursor) == Invalid)
        return CXChildVisit_Recurse;
    const Location loc(cursor);
    if (loc.path.isEmpty())
        return CXChildVisit_Recurse;
    QHash<QByteArray, ComprehensiveTreeUserDataNode> *hash = reinterpret_cast<QHash<QByteArray, ComprehensiveTreeUserDataNode> *>(data);
    const QByteArray id = loc.toString();
    QHash<QByteArray, ComprehensiveTreeUserDataNode>::iterator it = hash->find(id);
    if (it != hash->end())  {
        ComprehensiveTreeUserDataNode &node = it.value();
        switch (clang_getCursorKind(node.cursor)) {
        case CXCursor_ClassDecl:
        case CXCursor_StructDecl:
            if (clang_getCursorKind(node.hackParent) != CXCursor_TypedefDecl
                && clang_getCursorKind(hackParent) == CXCursor_TypedefDecl) {
                node.hackParent = hackParent;
            }
            break;
        default:
            break;
        }
        return CXChildVisit_Recurse;
    }

    CXCursor parent = clang_getCursorSemanticParent(cursor);
    while (clang_getCursorKind(parent) != CXCursor_TranslationUnit && isValidCursor(parent)
           && Node::nodeTypeFromCursor(parent) == Invalid) {
        parent = clang_getCursorSemanticParent(parent);
    }
    const Location parentLoc(parent);
    const QByteArray parentId = parentLoc.toString();
    const ComprehensiveTreeUserDataNode node = { id, loc, cursor, parent, hackParent, parentLoc, parentId };
    (*hash)[id] = node;
    return CXChildVisit_Recurse;
}

ClangRunnable::ClangRunnable(const Path &file, const ClangArgs &args)
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
    const time_t lastModified = mFile.lastModified();
    extern int verbose;
    if (verbose >= 2) {
        qDebug("%s", mArgs.toString(mFile.constData()).constData());
    }
    const bool hasPch = !mArgs.pchFile.isEmpty();
    CXTranslationUnit unit = 0;
    for (int i=0; i<2; ++i) {
        const int count = ((i == 1 && hasPch) ? mArgs.clangArgs.size() - 2 : mArgs.clangArgs.size());
        unit = clang_parseTranslationUnit(index, mFile.constData(),
                                          mArgs.clangArgs.constData(), count,
                                          0, 0, CXTranslationUnit_DetailedPreprocessingRecord
                                          |CXTranslationUnit_Incomplete); // ### do we need this?
        if (!unit) {
            if (i == 1 || !hasPch) {
                qWarning("Couldn't parse %s\n%s", mFile.constData(),
                         mArgs.toString(mFile.constData()).constData());
                break;
            }
            qDebug() << "retrying without pch" << mFile;
            continue;
        }
        Q_ASSERT(unit);
        const int diagnosticsCount = clang_getNumDiagnostics(unit);
        if (i == 0 && hasPch) {
            bool retry = false;
            for (int j=0; j<diagnosticsCount && !retry; ++j) {
                CXDiagnostic diagnostic = clang_getDiagnostic(unit, j);
                const CXDiagnosticSeverity severity = clang_getDiagnosticSeverity(diagnostic);
                if (severity >= CXDiagnostic_Error) {
                    if (verbose) {
                        const unsigned diagnosticFormattingOptions = (CXDiagnostic_DisplaySourceLocation|CXDiagnostic_DisplayColumn|
                                                                      CXDiagnostic_DisplaySourceRanges|CXDiagnostic_DisplayOption|
                                                                      CXDiagnostic_DisplayCategoryId|CXDiagnostic_DisplayCategoryName);

                        CXString diagStr = clang_formatDiagnostic(diagnostic, diagnosticFormattingOptions);
                        qWarning() << mFile << clang_getCString(diagStr);
                        clang_disposeString(diagStr);
                    }
                    retry = true;
                }
                clang_disposeDiagnostic(diagnostic);
            }
            if (retry) {
                qDebug() << "retrying without pch" << mFile;
                continue;
            }
        }
#ifdef QT_DEBUG
        for (int j=0; j<diagnosticsCount; ++j) {
            CXDiagnostic diagnostic = clang_getDiagnostic(unit, j);
            if (clang_getDiagnosticSeverity(diagnostic) > (verbose ? CXDiagnostic_Note : CXDiagnostic_Warning)) {
                CXString diagStr = clang_getDiagnosticSpelling(diagnostic);
                const unsigned diagnosticFormattingOptions = (CXDiagnostic_DisplaySourceLocation|CXDiagnostic_DisplayColumn|
                                                              CXDiagnostic_DisplaySourceRanges|CXDiagnostic_DisplayOption|
                                                              CXDiagnostic_DisplayCategoryId|CXDiagnostic_DisplayCategoryName);

                CXString diagStr2 = clang_formatDiagnostic(diagnostic, diagnosticFormattingOptions);
                qWarning() << mFile << clang_getCString(diagStr) << clang_getCString(diagStr2) << clang_getDiagnosticSeverity(diagnostic);
                clang_disposeString(diagStr);
                clang_disposeString(diagStr2);
            }
            clang_disposeDiagnostic(diagnostic);
        }
#endif
        break;
    }

    if (unit) {
        PrecompileData pre;
        clang_getInclusions(unit, precompileHeaders, &pre);
        {
#warning we already have the dependencies in RBuild
            // if (precompile) {
            //     QMutexLocker lock(&sPCHMutex);
            //     precompile->add(pre.direct, pre.all);
            // }
            QMutexLocker lock(&sFilesMutex);
            FileData &data = sFiles[mFile];
            data.lastModified = lastModified;
#warning need a solution for this too
            // data.arguments = mArgs;
            foreach(const Path &dependency, pre.all) {
                data.dependencies[dependency] = dependency.lastModified(); // ### raise condition, only checking time after parsing
            }
        }
        processTranslationUnit(mFile, unit);
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
    Q_ASSERT(nodePosition > 0);
    device->seek(nodePosition);
    writeInt32(device, node->type);
    const int32_t location = (entryIdx == -1 ? 0 : (entryIdx * entryLength) + FirstId + Int32Length);
    writeInt32(device, location);
    writeInt32(device, positions.value(node->parent, 0));
    writeInt32(device, positions.value(node->nextSibling, 0));
    if (node->firstChild && node->containingFunction) {
        qWarning() << "bug here" << node->toString() << node->firstChild->toString()
                   << node->containingFunction->toString();
    }
    Q_ASSERT(!(node->firstChild && node->containingFunction));
    Q_ASSERT(!node->containingFunction || node->type == Reference);
    if (node->type == Reference) {
        writeInt32(device, positions.value(node->containingFunction, 0));
    } else {
        writeInt32(device, positions.value(node->firstChild, 0));
    }
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
                if (!parent->symbolName.isEmpty()) {
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

int ClangRunnable::initTree(const MMapData *data, const QSet<Path> &modifiedPaths)
{
    QMutexLocker lock(&sTreeMutex);
    Q_ASSERT(sRoot);
    // qDebug() << modifiedPaths;
    const NodeData nodeData = readNodeData(data->memory + data->rootNodePosition);
    QHash<Node*, QByteArray> pendingContainingFunctions;
    const int ret = initTree(data, modifiedPaths, sRoot, nodeData, pendingContainingFunctions);
    for (QHash<Node*, QByteArray>::const_iterator it = pendingContainingFunctions.begin();
         it != pendingContainingFunctions.end(); ++it) {
        it.key()->containingFunction = Node::sNodes.value(it.value());
        if (!it.key()->containingFunction) {
            qWarning() << "Couldn't restore containingFunction for" << it.key()->toString();
        }
    }
    return ret;
}

int ClangRunnable::initTree(const MMapData *data, const QSet<Path> &modifiedPaths,
                            Node *parent, const NodeData &nodeData,
                            QHash<Node*, QByteArray> &pendingContainingFunctions)
{
    int ret = 0;
    int child = nodeData.type == Reference ? 0 : nodeData.firstChild;
    while (child) {
        const NodeData c = readNodeData(data->memory + child);
        const Location loc(data->memory + c.location);
        if (!modifiedPaths.contains(loc.path)) {
            const QByteArray id = loc.toString();
            Node *node = Node::sNodes.value(id);
            if (!node) {
                extern int verbose;
                node = new Node(parent, static_cast<NodeType>(c.type), QByteArray(c.symbolName), loc, id);
                if (verbose)
                    qDebug() << "creating node from database" << node->toString();
                ++ret;
                if (c.type == Reference && c.containingFunction) {
                    const NodeData containingFunction = readNodeData(data->memory + c.containingFunction);
                    const Location cfl(data->memory + containingFunction.location);
                    const QByteArray cfid = cfl.toString();
                    node->containingFunction = Node::sNodes.value(cfid);
                    if (!node->containingFunction) {
                        pendingContainingFunctions[node] = cfid;
                    }
                }
            }
            ret += initTree(data, modifiedPaths, node, c, pendingContainingFunctions);
        }
        child = c.nextSibling;
    }
    return ret;
}

static inline Node *findContainingFunction(CXCursor cursor)
{
    CXCursor parent = clang_getCursorSemanticParent(cursor);
    while (isValidCursor(parent)) {
        switch (clang_getCursorKind(parent)) {
        case CXCursor_FunctionDecl:
        case CXCursor_CXXMethod:
            return Node::sNodes.value(Location(parent).toString());
        default:
            break;
        }
        parent = clang_getCursorSemanticParent(parent);
    }
    return 0;
}

int ClangRunnable::processTranslationUnit(const Path &file, CXTranslationUnit unit)
{
    QElapsedTimer timer;
    timer.start();
    int ret = 0;
    Q_ASSERT(unit);
    Q_UNUSED(file);
    CXCursor rootCursor = clang_getTranslationUnitCursor(unit);
    QHash<QByteArray, ComprehensiveTreeUserDataNode> hash;
    const QByteArray dump = qgetenv("RTAGS_DUMP");
    if (dump == "1" || dump.contains(file.fileName())) {
        clang_visitChildren(rootCursor, dumpTree, 0);
        fflush(stdout);
        sleep(1);
    }
    clang_visitChildren(rootCursor, buildComprehensiveTree, &hash);
    QMutexLocker lock(&sTreeMutex);

    // ### nasty data structures here
    bool doReferences = false;
    forever {
        bool createdNodes = false;
        // qDebug() << iteration++ << Node::sNodes.size() << hash.size() << before;

        QHash<QByteArray, ComprehensiveTreeUserDataNode>::iterator it = hash.begin();
        while (it != hash.end()) {
            ComprehensiveTreeUserDataNode &node = *it;
            if (Node::sNodes.contains(node.id)) {
                // qWarning() << node.cursor << "already exists";
                it = hash.erase(it);
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
                            Q_ASSERT(referenced->type != Reference);
                            break;
                        }
                    }
                    if (!referenced) {
                        it = hash.erase(it);
                        continue;
                    }
                } else {
                    const CXCursorKind kind = clang_getCursorKind(node.cursor);

                    CXCursor ref = clang_getCursorReferenced(node.cursor);
                    if (clang_equalCursors(ref, node.cursor)) {
                        bool selfRef = true;
                        if (kind == CXCursor_ClassDecl || kind == CXCursor_StructDecl) {
                            // ### namespace too?
                            ref = clang_getCursorDefinition(ref);
                            selfRef = clang_equalCursors(ref, node.cursor);
                        }
                        if (selfRef) {
                            qWarning() << "This cursor references itself" << node.cursor;
                            continue;
                        }
                    }

                    if (!isValidCursor(ref)) {
                        switch (kind) {
                        case CXCursor_MacroExpansion:
                        case CXCursor_ClassDecl:
                        case CXCursor_StructDecl:
                        case CXCursor_DeclRefExpr:
                        case CXCursor_MemberRefExpr: // ### these annoy me, not
                                                     // ### sure why they so
                                                     // ### often don't work
                            break;
                        default:
                            qWarning() << "Can't get valid cursor for" << node.cursor
                                       << "parent" << clang_getCursorSemanticParent(node.cursor);
                            break;
                        }
                        it = hash.erase(it);
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
                            it = hash.erase(it);
                            continue;
                        default:
                            break;
                        }
                    default:
                        break;
                    }
                    referenced = Node::sNodes.value(refId);
                    if (!referenced) {
                        // qWarning() << "Can't find referenced node" << node.cursor << ref << refId;
                        it = hash.erase(it);
                        continue;
                    }
                    if (referenced->type == MethodDefinition) {
                        Node *decl = referenced->methodDeclaration();
                        if (decl)
                            referenced = decl;
                    }
                    if (referenced->type == Reference) {
                        it = hash.erase(it);
                        continue;
                    }
                }
                Q_ASSERT(referenced->location != node.loc);
                Node *n = new Node(referenced, Reference, referenced->symbolName, node.loc, node.id);
                n->containingFunction = findContainingFunction(node.cursor);
                ++ret;
                it = hash.erase(it);
            } else { // non-references
                Node *p = 0;
                if (!node.parentId.isEmpty()) {
                    p = Node::sNodes.value(node.parentId);
                    if (!p) {
                        // if (i)
                        //     qWarning() << "Can't find parent" << node.cursor << node.parent;
                        ++it;
                        continue;
                    } else if (p->type == Reference) {
                        it = hash.erase(it);
                        continue;
                    }
                } else {
                    p = sRoot;
                }

                Q_ASSERT(type != Reference);
                QByteArray symbolName = eatString(clang_getCursorDisplayName(node.cursor));
                if (symbolName.isEmpty() && (type == Struct || type == Class)
                    && clang_getCursorKind(node.hackParent) == CXCursor_TypedefDecl) {
                    symbolName = eatString(clang_getCursorDisplayName(node.hackParent));
                }
                new Node(p, type, symbolName, node.loc, node.id);
                ++ret;
                createdNodes = true;
                it = hash.erase(it);
            }
        }
        if (doReferences)
            break;
        if (!createdNodes)
            doReferences = true;
    }
    const int elapsed = timer.elapsed();
    extern int verbose;
    if (verbose)
        qDebug("Compiled %s, %d new nodes in %dms", file.constData(), ret, elapsed);
    return ret;
}
