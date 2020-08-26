#include "Node.h"

#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <utility>

#include "TranslationUnit.h"
#include "clang-c/Index.h"

static inline CXType clang_getNullType()
{
    CXType t;
    memset(&t, 0, sizeof(t));
    return t;
}

static_assert(Node::TemplateArgument == 1ull << Node::FirstBit, "These must match");
static_assert(Node::ArgumentType == 1ull << Node::LastBit, "These must match");

Node::Node(Node *parent, NodeType nodeType, const CXCursor &cursor)
    : mNodeType(nodeType), mParent(parent), mFlags(parent->mFlags & NodeFlagsMask), mClangCursor(cursor), mClangType(clang_getNullType())
{
    assert(!clang_isInvalid(clang_getCursorKind(cursor)));
    static_assert(static_cast<int>(ShowDefines) == TranslationUnit::ShowDefines,
                  "These should be kept in sync");
    static_assert(static_cast<int>(ShowIncludes) == TranslationUnit::ShowIncludes,
                  "These should be kept in sync");
    static_assert(static_cast<int>(ShowTypedefs) == TranslationUnit::ShowTypedefs,
                  "These should be kept in sync");
}

Node::Node(Node *parent, NodeType nodeType, const CXType &type)
    : mNodeType(nodeType), mParent(parent), mFlags(parent->mFlags & NodeFlagsMask), mClangCursor(clang_getNullCursor()), mClangType(type)
{
    // assert(type.kind != CXType_Invalid);
}

Node::Node(const CXCursor &cursor, unsigned int flags)
    : mNodeType(Root), mParent(nullptr), mFlags(flags), mClangCursor(cursor), mClangType(clang_getNullType())
{
}

Node::~Node()
{
}

Node *Node::canonical()
{
    return impl(Canonical, [this]() {
        return clang_getCanonicalCursor(mClangCursor);
    });
}

Node *Node::semanticParent()
{
    return impl(SemanticParent, [this]() -> CXCursor {
        const CXCursor cursor = clang_getCursorSemanticParent(mClangCursor);
        if (isValid(cursor) && (!mParent || !clang_equalCursors(mParent->mClangCursor, cursor))) {
            return cursor;
        }
        return clang_getNullCursor();
    });
}

Node *Node::lexicalParent()
{
    return impl(LexicalParent, [this]() -> CXCursor {
        const CXCursor cursor = clang_getCursorLexicalParent(mClangCursor);
        if (isValid(cursor) && (!mParent || !clang_equalCursors(mParent->mClangCursor, cursor))) {
            return cursor;
        }
        return clang_getNullCursor();
    });
}

Node *Node::definition()
{
    return impl(Definition, [this]() -> CXCursor {
        return clang_getCursorDefinition(mClangCursor);
    });
}

Node *Node::specializedCursorTemplate()
{
    return impl(SpecializedCursorTemplate, [this]() -> CXCursor {
        return clang_getSpecializedCursorTemplate(mClangCursor);
    });
}

Node *Node::type()
{
    return impl(Type, [this]() -> CXType {
        return clang_getCursorType(mClangCursor);
    });
}

Node *Node::typedefUnderlyingType()
{
    return impl(TypedefUnderlyingType, [this]() -> CXType {
        return clang_getTypedefDeclUnderlyingType(mClangCursor);
    });
}

Node *Node::enumIntegerType()
{
    return impl(EnumIntegerType, [this]() -> CXType {
        return clang_getEnumDeclIntegerType(mClangCursor);
    });
}

Node *Node::canonicalType()
{
    return impl(CanonicalType, [this]() -> CXType {
        return clang_getCanonicalType(mClangType);
    });
}

Node *Node::pointeeType()
{
    return impl(PointeeType, [this]() -> CXType {
        return clang_getPointeeType(mClangType);
    });
}

Node *Node::typeDeclaration()
{
    return impl(TypeDeclaration, [this]() -> CXCursor {
        return clang_getTypeDeclaration(mClangType);
    });
}

Node *Node::resultType()
{
    return impl(Result, [this]() -> CXType {
        CXType type = clang_getResultType(mClangType);
        if (!isValid(type))
            type = clang_getCursorResultType(mClangCursor);
        return type;
    });
}

Node *Node::elementType()
{
    return impl(Result, [this]() -> CXType {
        CXType type = clang_getResultType(mClangType);
        if (!isValid(type))
            type = clang_getCursorResultType(mClangCursor);
        return type;
    });
}

Node *Node::reference()
{
    return impl(Reference, [this]() {
        return clang_getCursorReferenced(mClangCursor);
    });
}

const std::vector<Node *> &Node::children()
{
    if (!(mFlags & Children)) {
        mFlags |= Children;
        clang_visitChildren(mClangCursor, [](CXCursor cursor, CXCursor, void *userData) {
            Node *that = static_cast<Node *>(userData);
            switch (clang_getCursorKind(cursor)) {
            case CXCursor_MacroDefinition:
            case CXCursor_MacroExpansion:
                if (!(that->mFlags & ShowDefines)) {
                    return CXChildVisit_Continue;
                }
                break;
            case CXCursor_InclusionDirective:
                if (!(that->mFlags & ShowIncludes)) {
                    return CXChildVisit_Continue;
                }
                break;
            case CXCursor_TypedefDecl:
                if (!(that->mFlags & ShowTypedefs)) {
                    return CXChildVisit_Continue;
                }
                break;
            default:
                break;
            }
            that->mChildren.push_back(new Node(that, Children, cursor));
            return CXChildVisit_Continue;
        }, this);
    }

    return mChildren;
}

const std::vector<Node *> &Node::argumentTypes()
{
    if (!(mFlags & ArgumentType)) {
        mFlags |= ArgumentType;
        const int count = clang_getNumArgTypes(mClangType);
        if (count > 0) {
            mArgumentTypes.resize(count);
            for (int i=0; i<count; ++i) {
                mArgumentTypes[i] = new Node(this, ArgumentType, clang_getArgType(mClangType, i));
            }
        }
    }
    return mArgumentTypes;
}

const std::vector<Node *> &Node::arguments()
{
    if (!(mFlags & Argument)) {
        mFlags |= Argument;
        const int count = clang_Cursor_getNumArguments(mClangCursor);
        if (count > 0) {
            mArguments.resize(count);
            for (int i=0; i<count; ++i) {
                mArguments[i] = new Node(this, Argument, clang_Cursor_getArgument(mClangCursor, i));
            }
        }
    }
    return mArguments;
}

const std::vector<Node *> &Node::templateArguments()
{
    if (!(mFlags & TemplateArgument)) {
        mFlags |= TemplateArgument;
        if (isValid(mClangType)) {
            const int count = clang_Type_getNumTemplateArguments(mClangType);
            if (count > 0) {
                mTemplateArguments.resize(count);
                for (int i=0; i<count; ++i) {
                    mTemplateArguments[i] = new Node(this, TemplateArgument, clang_Type_getTemplateArgumentAsType(mClangType, i));
                }
            }
        } else {
            const int count = clang_Cursor_getNumTemplateArguments(mClangCursor);
            if (count > 0) {
                mTemplateArguments.resize(count);
                for (int i=0; i<count; ++i) {
                    auto a = clang_Cursor_getTemplateArgumentType(mClangCursor, i);
                    auto b = clang_Cursor_getTemplateArgumentType(clang_getSpecializedCursorTemplate(mClangCursor), i);
                    if (a.kind == CXType_Invalid) {
                        printf("SHIT %d %d\n", a.kind, b.kind);
                    }

                    mTemplateArguments[i] = new Node(this, TemplateArgument, clang_Cursor_getTemplateArgumentType(mClangCursor, i));
                }
            }
        }
    }
    return mArguments;
}


const std::vector<Node *> &Node::overloadedDecls()
{
    if (!(mFlags & OverloadedDeclaration)) {
        mFlags |= OverloadedDeclaration;
        const int count = clang_getNumOverloadedDecls(mClangCursor);
        if (count > 0) {
            mOverloadedDecls.resize(count);
            for (int i=0; i<count; ++i) {
                mOverloadedDecls[i] = new Node(this, OverloadedDeclaration, clang_getOverloadedDecl(mClangCursor, i));
            }
        }
    }
    return mOverloadedDecls;
}

void Node::deleteRecursive(Node *node, std::unordered_set<Node *> &seen)
{
    if (!node)
        return;

    if (!seen.insert(node).second) {
        return;
    }
    for (int i = Node::FirstBit; i<=Node::LastBit; ++i) {
        Node::NodeType nodeType = static_cast<Node::NodeType>(1ull << i);
        Node **child = nullptr;
        std::vector<Node *> *children = nullptr;
        node->variable(nodeType, &child, &children);
        if (child && *child) {
            deleteRecursive(*child, seen);
        } else if (children) {
            for (Node *c : *children) {
                deleteRecursive(c, seen);
            }
        }
    }

    delete node;
}

Node *Node::impl(NodeType nodeType, std::function<CXType()> &&typeFunc)
{
    Node **var = nullptr;
    variable(nodeType, &var, nullptr);
    if (!(mFlags & nodeType)) {
        assert(!*var);
        mFlags |= nodeType;
        CXType type = typeFunc();
        if (isValid(type) && !clang_equalTypes(type, mClangType)) {
            *var = new Node(this, nodeType, type);
        }
    }
    return *var;
}

Node *Node::impl(NodeType nodeType, std::function<CXCursor()> &&cursorFunc,
                 std::function<CXType()> &&typeFunc)
{
    Node **var = nullptr;
    variable(nodeType, &var, nullptr);
    if (!(mFlags & nodeType)) {
        mFlags |= nodeType;
        CXCursor cursor = cursorFunc();
        if (isValid(cursor) && !clang_equalCursors(cursor, mClangCursor)) {
            *var = new Node(this, nodeType, cursor);
        } else if (typeFunc) {
            CXType type = typeFunc();
            if (isValid(type) && !clang_equalTypes(type, mClangType)) {
                *var = new Node(this, nodeType, type);
            }
        }
    }
    return *var;
}

void Node::variable(NodeType nodeType,
                    Node ***variable,
                    std::vector<Node *> **vector,
                    std::function<void()> *init)
{
    assert(variable);
    assert(!vector || !*vector);
    switch (nodeType) {
    case Root: assert(0); break;
    case Children: *vector = &mChildren; initializer(init, &Node::children); break;
    case Argument: *vector = &mArguments; initializer(init, &Node::arguments); break;
    case ArgumentType: *vector = &mArgumentTypes; initializer(init, &Node::argumentTypes); break;
    case OverloadedDeclaration: *vector = &mOverloadedDecls; initializer(init, &Node::overloadedDecls); break;
    case TemplateArgument: *vector = &mTemplateArguments; initializer(init, &Node::templateArguments); break;
    case Reference: *variable = &mReference; initializer(init, &Node::reference); break;
    case SemanticParent: *variable = &mSemanticParent; initializer(init, &Node::semanticParent); break;
    case LexicalParent: *variable = &mLexicalParent; initializer(init, &Node::lexicalParent); break;
    case Definition: *variable = &mDefinition; initializer(init, &Node::definition); break;
    case Canonical: *variable = &mCanonical; initializer(init, &Node::canonical); break;
    case SpecializedCursorTemplate: *variable = &mSpecializedCursorTemplate; initializer(init, &Node::specializedCursorTemplate); break;
    case TypedefUnderlyingType: *variable = &mTypedefUnderlyingType; initializer(init, &Node::typedefUnderlyingType); break;
    case EnumIntegerType: *variable = &mEnumIntegerType; initializer(init, &Node::enumIntegerType); break;
    case CanonicalType: *variable = &mCanonicalType; initializer(init, &Node::canonicalType); break;
    case PointeeType: *variable = &mPointeeType; initializer(init, &Node::pointeeType); break;
    case Result: *variable = &mResult; initializer(init, &Node::resultType); break;
    case ElementType: *variable = &mElementType; initializer(init, &Node::elementType); break;
    case TypeDeclaration: *variable = &mTypeDeclaration; initializer(init, &Node::typeDeclaration); break;
    case Type: *variable = &mType; initializer(init, &Node::type); break;
    }
}

void Node::extract(NodeType nodeType, Node ***child, std::vector<Node *> **childVector)
{
    std::function<void()> init;
    variable(nodeType, child, childVector, &init);
    init();
}
