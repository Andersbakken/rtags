#include <clang-c/Index.h>
#include <string.h>
#include <stdlib.h>
#include <ByteArray.h>
#include <Timer.h>
#include <Log.h>
#include <RTags.h>
#include <Location.h>

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

    void dump(int indent = 0);

    Location location;
    CXCursorKind kind;
    ByteArray symbolName, usr, referencedUsr;
    Node *firstChild, *lastChild, *prev, *next, *parent;
};

static inline CXChildVisitResult visitAll(CXCursor cursor, CXCursor, CXClientData userData)
{
    Node *parent = reinterpret_cast<Node*>(userData);
    Node *node = new Node(cursor, parent);
    clang_visitChildren(cursor, visitAll, node);
    return CXChildVisit_Continue;
}

Node::Node(const CXCursor &cursor, Node *p)
    : location(createLocation(cursor)), kind(clang_getCursorKind(cursor)),
      symbolName(RTags::eatString(clang_getCursorDisplayName(cursor))),
      usr(RTags::eatString(clang_getCursorUSR(cursor))),
      firstChild(0), lastChild(0), prev(0), next(0), parent(p)
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
    ++indent;
    for (Node *n=firstChild; n; n = n->next) {
        n->dump(indent);
    }
}

int main(int argc, char **argv)
{
    Timer timer;
    CXIndex index = clang_createIndex(1, 1);
    const char *args[] = { "-I.", "-x", "c++" }; //, "-include-pch", "/tmp/pch.pch" };
    CXTranslationUnit unit = clang_parseTranslationUnit(index, "test.cpp",
                                                        args, sizeof(args) / sizeof(char*),
                                                        0, 0, clang_defaultEditingTranslationUnitOptions());
    if (unit) {
        CXCursor rootCursor(clang_getTranslationUnitCursor(unit));
        Node root(rootCursor, 0);
        clang_visitChildren(clang_getTranslationUnitCursor(unit), visitAll, &root);
        root.dump();
        clang_disposeTranslationUnit(unit);
    }
    clang_disposeIndex(index);
    return 0;
}
