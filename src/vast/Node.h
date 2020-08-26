#ifndef NODE_H
#define NODE_H

#include <clang-c/Index.h>
#include <stdint.h>
#include <vector>
#include <unordered_set>
#include <functional>

inline bool isValid(const CXCursor &cursor)
{
    return !clang_isInvalid(clang_getCursorKind(cursor));
}

inline bool isValid(const CXType &type)
{
    return type.kind != CXType_Invalid;
}

class Node
{
public:
    enum NodeType {
        Root = 1ull << 16,
        TemplateArgument = 1ull << 17,
        Result = 1ull << 18,
        Argument = 1ull << 19,
        Children = 1ull << 20,
        Reference = 1ull << 21,
        SemanticParent = 1ull << 22,
        LexicalParent = 1ull << 23,
        Definition = 1ull << 24,
        Canonical = 1ull << 25,
        SpecializedCursorTemplate = 1ull << 26,
        TypedefUnderlyingType = 1ull << 27,
        EnumIntegerType = 1ull << 28,
        CanonicalType = 1ull << 29,
        PointeeType = 1ull << 30,
        ElementType = 1ull << 31,
        OverloadedDeclaration = 1ull << 32,
        TypeDeclaration = 1ull << 33,
        Type = 1ull << 34,
        ArgumentType = 1ull << 35
    };
    enum {
        FirstBit = 17,
        LastBit = 35
    };
    Node(Node *parent, NodeType nodeType, const CXCursor &cursor);
    Node(Node *parent, NodeType nodeType, const CXType &type);
    Node(const CXCursor &cursor, unsigned int flags);
    ~Node();

    void extract(NodeType nodeType, Node ***child, std::vector<Node *> **children);

    const CXCursor &clangCursor() const { return mClangCursor; }
    const CXType &clangType() const { return mClangType; }
    uintptr_t data() const { return mData; }
    void setData(uintptr_t data) { mData = data; }

    NodeType nodeType() const { return mNodeType; }
    Node *parent() const { return mParent; }

    Node *canonical();
    Node *canonicalType();
    Node *type();
    Node *definition();
    Node *elementType();
    Node *enumIntegerType();
    Node *lexicalParent();
    Node *pointeeType();
    Node *reference();
    Node *resultType();
    Node *semanticParent();
    Node *specializedCursorTemplate();
    Node *typeDeclaration();
    Node *typedefUnderlyingType();
    const std::vector<Node *> &argumentTypes();
    const std::vector<Node *> &arguments();
    const std::vector<Node *> &children();
    const std::vector<Node *> &overloadedDecls();
    const std::vector<Node *> &templateArguments();

    static void deleteRecursive(Node *node, std::unordered_set<Node *> &seen);
private:
    template <typename T>
    void initializer(std::function<void()> *init, T t)
    {
        if (init) {
            *init = [this, t]() {
                ((*this).*(t))();
            };
        }
    }

    void variable(NodeType nodeType, Node ***variable, std::vector<Node *> **vector,
                  std::function<void()> *init = nullptr);
    Node *impl(NodeType t, std::function<CXType()> &&typeFunc);
    Node *impl(NodeType t, std::function<CXCursor()> &&cursorFunc,
               std::function<CXType()> &&typeFunc = nullptr);

    // Some of these flags are duplicated from TranslationUnit
    enum Flag {
        ShowDefines = 1 << 1,
        ShowIncludes = 1 << 2,
        ShowTypedefs = 1 << 3,
        NodeFlagsMask = (ShowTypedefs|ShowIncludes|ShowDefines)
    };
    uintptr_t mData { 0 };
    const NodeType mNodeType;
    Node *mParent { nullptr };
    unsigned long long mFlags;
    std::vector<Node *> mArgumentTypes, mArguments, mChildren, mOverloadedDecls, mTemplateArguments;
    Node *mCanonical { nullptr };
    Node *mCanonicalType { nullptr };
    Node *mCursor { nullptr };
    Node *mCursorType { nullptr };
    Node *mDefinition { nullptr };
    Node *mElementType { nullptr };
    Node *mEnumIntegerType { nullptr };
    Node *mLexicalParent { nullptr };
    Node *mPointeeType { nullptr };
    Node *mReference { nullptr };
    Node *mResult { nullptr };
    Node *mSemanticParent { nullptr };
    Node *mSpecializedCursorTemplate { nullptr };
    Node *mDeclaration { nullptr };
    Node *mTypedefUnderlyingType { nullptr };
    Node *mTypeDeclaration { nullptr };
    Node *mType { nullptr };

    const CXCursor mClangCursor;
    const CXType mClangType;
};


#endif /* NODE_H */
