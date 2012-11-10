#include <clang-c/Index.h>
#include <string.h>
#include <stdlib.h>
#include "ByteArray.h"
#include "Timer.h"
#include "Log.h"
#include "RTagsClang.h"
#include "RTags.h"
#include "Location.h"
#include "GccArguments.h"

static const CXSourceLocation nullLocation = clang_getNullLocation();
static inline Location createLocation(const CXCursor &cursor)
{
    CXSourceLocation location = clang_getCursorLocation(cursor);
    if (!clang_equalLocations(location, nullLocation)) {
        CXFile file;
        unsigned start;
        clang_getSpellingLocation(location, &file, 0, 0, &start);
        if (file)
            return Location(file, start);
    }
    return Location();
}

struct Node
{
    Node(const CXCursor &cursor, Node *parent);
    ~Node();

    void dump(int indent = -1);

    Location location;
    CXCursorKind kind;
    ByteArray symbolName, usr, referencedUsr;
    Node *firstChild, *lastChild, *prev, *next, *parent;
    CXCursor cursor;
};

struct UserData
{
    Node *parent;
    Map<Location, List<Node*> > &nodes;
};

static inline CXChildVisitResult visitAll(CXCursor cursor, CXCursor, CXClientData userData)
{
    UserData *u = reinterpret_cast<UserData*>(userData);
    Node *node = new Node(cursor, u->parent);
    u->nodes[node->location].append(node);
    UserData u2 = { node, u->nodes };
    clang_visitChildren(cursor, visitAll, &u2);
    return CXChildVisit_Continue;
}

Node::Node(const CXCursor &c, Node *p)
    : location(createLocation(c)), kind(clang_getCursorKind(c)),
      symbolName(RTags::eatString(clang_getCursorDisplayName(c))),
      usr(RTags::eatString(clang_getCursorUSR(c))),
      firstChild(0), lastChild(0), prev(0), next(0), parent(p), cursor(c)
{
    CXCursor ref = clang_getCursorReferenced(cursor);
    if (!clang_isInvalid(clang_getCursorKind(ref)))
        referencedUsr = RTags::eatString(clang_getCursorUSR(ref));
    if (parent) {
        if (!parent->firstChild) {
            parent->firstChild = parent->lastChild = this;
        } else {
            parent->lastChild->next = this;
            prev = parent->lastChild;
            parent->lastChild = this;
        }
    }
}

Node::~Node()
{
    while (firstChild) {
        Node *n = firstChild->next;
        delete firstChild;
        firstChild = n;
    }
}

void Node::dump(int indent)
{
    if (location.isValid()) {
        const char *ws = "  ";
        for (int i=0; i<indent; ++i)
            printf("%s", ws);
        const ByteArray ret = RTags::eatString(clang_getCursorKindSpelling(kind));
        printf("%s", ret.constData());
        if (!symbolName.isEmpty())
            printf(" %s", symbolName.constData());
        if (!location.isNull())
            printf(" %s", location.key().constData());
        if (!usr.isEmpty())
            printf(" %s", usr.constData());
        if (!referencedUsr.isEmpty())
            printf(" %s", referencedUsr.constData());
        printf("\n");
    }
    if (indent >= 0) {
        ++indent;
        for (Node *n=firstChild; n; n = n->next) {
            n->dump(indent);
        }
    }
}


static inline const char *kindToString(CXIdxEntityKind kind)
{
    switch (kind) {
    case CXIdxEntity_CXXInterface: return "CXXInterface";
    case CXIdxEntity_Unexposed: return "Unexposed";
    case CXIdxEntity_Typedef: return "Typedef";
    case CXIdxEntity_Function: return "Function";
    case CXIdxEntity_Variable: return "Variable";
    case CXIdxEntity_Field: return "Field";
    case CXIdxEntity_EnumConstant: return "EnumConstant";
    case CXIdxEntity_ObjCClass: return "ObjCClass";
    case CXIdxEntity_ObjCProtocol: return "ObjCProtocol";
    case CXIdxEntity_ObjCCategory: return "ObjCCategory";
    case CXIdxEntity_ObjCInstanceMethod: return "ObjCInstanceMethod";
    case CXIdxEntity_ObjCClassMethod: return "ObjCClassMethod";
    case CXIdxEntity_ObjCProperty: return "ObjCProperty";
    case CXIdxEntity_ObjCIvar: return "ObjCIvar";
    case CXIdxEntity_Enum: return "Enum";
    case CXIdxEntity_Struct: return "Struct";
    case CXIdxEntity_Union: return "Union";
    case CXIdxEntity_CXXClass: return "CXXClass";
    case CXIdxEntity_CXXNamespace: return "CXXNamespace";
    case CXIdxEntity_CXXNamespaceAlias: return "CXXNamespaceAlias";
    case CXIdxEntity_CXXStaticVariable: return "CXXStaticVariable";
    case CXIdxEntity_CXXStaticMethod: return "CXXStaticMethod";
    case CXIdxEntity_CXXInstanceMethod: return "CXXInstanceMethod";
    case CXIdxEntity_CXXConstructor: return "CXXConstructor";
    case CXIdxEntity_CXXDestructor: return "CXXDestructor";
    case CXIdxEntity_CXXConversionFunction: return "CXXConversionFunction";
    case CXIdxEntity_CXXTypeAlias: return "CXXTypeAlias";
    }
    return "";
}

void indexDeclaration(CXClientData, const CXIdxDeclInfo *decl)
{
    CXFile f;
    unsigned l, c, o;
    clang_indexLoc_getFileLocation(decl->loc, 0, &f, &l, &c, &o);
    char buf[1024];
    if (!getcwd(buf, sizeof(buf)))
        return;
    printf("%s/%s,%d %s %s\n",
           buf, RTags::eatString(clang_getFileName(f)).constData(),
           o, kindToString(decl->entityInfo->kind), decl->entityInfo->name);
    switch (decl->entityInfo->kind) {
    case CXIdxEntity_Field:
    case CXIdxEntity_Variable:
    case CXIdxEntity_Function:
    case CXIdxEntity_CXXInstanceMethod:
    case CXIdxEntity_CXXConstructor:
        // clang_visitChildren(decl->cursor, visitor, 0);
        break;
    default:
        break;
    }
}

void indexEntityReference(CXClientData, const CXIdxEntityRefInfo *ref)
{
    CXFile f;
    unsigned l, c, o;
    clang_indexLoc_getFileLocation(ref->loc, 0, &f, &l, &c, &o);

    CXSourceLocation loc = clang_getCursorLocation(ref->referencedEntity->cursor);
    CXFile f2;
    unsigned l2, c2, o2;
    clang_getInstantiationLocation(loc, &f2, &l2, &c2, &o2);
    char buf[1024];
    if (!getcwd(buf, sizeof(buf)))
        return;
    printf("%s/%s,%d ref of %s/%s,%d %s %s\n", buf,
           RTags::eatString(clang_getFileName(f)).constData(), o,
           buf, RTags::eatString(clang_getFileName(f2)).constData(), o2,
           kindToString(ref->referencedEntity->kind),
           ref->referencedEntity->name);
}

int main(int argc, char **argv)
{
    RTags::findApplicationDirPath(*argv);
    Timer timer;
    CXIndex index = clang_createIndex(1, 1);
    ByteArray ba;
    for (int i=1; i<argc; ++i) {
        if (i > 1) {
            ba.append(' ');
        }
        ba.append(argv[i]);
    }

    GccArguments args;
    if (args.parse(ba, RTags::applicationDirPath())) {
        const List<ByteArray> clangArgs = args.clangArgs();
        const char **a = new const char*[clangArgs.size()];
        for (int i=0; i<clangArgs.size(); ++i)
            a[i] = clangArgs.at(i).constData();
        CXTranslationUnit unit = clang_parseTranslationUnit(index, args.inputFiles().first().constData(),
                                                            a, clangArgs.size(),
                                                            0, 0, clang_defaultEditingTranslationUnitOptions());
        if (unit) {
            CXCursor rootCursor(clang_getTranslationUnitCursor(unit));
            Node root(rootCursor, 0);
            Map<Location, List<Node*> > nodes;
            UserData u = { &root, nodes };
            clang_visitChildren(clang_getTranslationUnitCursor(unit), visitAll, &u);
            root.dump(0);
            for (Map<Location, List<Node*> >::const_iterator it = nodes.begin(); it != nodes.end(); ++it) {
                // if (it->second.size() > 1) {
                printf("--------Got %d cursor(s) on %s----------\n",
                       it->second.size(), it->first.key().constData());
                for (int i=0; i<it->second.size(); ++i) {
                    printf("%s\n", RTags::cursorToString(it->second.at(i)->cursor, RTags::AllCursorToStringFlags).constData());
                }
            }
            clang_disposeTranslationUnit(unit);
            unit = 0;
        }
        // clang_IndexAction_create(index);
        // CXIndexAction b;
        // CXIndexAction action = clang_IndexAction_create(index);
        // IndexerCallbacks cb;
        // memset(&cb, 0, sizeof(IndexerCallbacks));
        // cb.indexDeclaration = indexDeclaration;
        // cb.indexEntityReference = indexEntityReference;

        // const char* filename = (argc < 2 ? "test.cpp" : argv[1]);

        // clang_indexSourceFile(action, 0, &cb, sizeof(IndexerCallbacks),
        //                       CXIndexOpt_IndexFunctionLocalSymbols,
        //                       filename,
        //                       args, sizeof(args) / sizeof(args[0]),
        //                       0, 0, &unit, clang_defaultEditingTranslationUnitOptions());

        clang_disposeIndex(index);
    }
    return 0;
}
