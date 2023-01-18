#include "Model.h"

#include <assert.h>
#include <qbytearray.h>
#include <qdebug.h>
#include <qfile.h>
#include <qiodevice.h>
#include <stddef.h>
#include <utility>
#include <vector>

#include "Node.h"
#include "clang-c/Index.h"
#include "qglobal.h"

class QObject;

enum Columns {
    DisplayName,
    NodeType,
    Kind,
    Location,
    ColumnCount
};

inline static QString toString(CXSourceLocation sourceLocation)
{
    if (clang_equalLocations(sourceLocation, clang_getNullLocation()))
        return QString();

    CXFile file { nullptr };
    unsigned line { 0 };
    unsigned column { 0 };
    clang_getSpellingLocation(sourceLocation,
                              &file,
                              &line,
                              &column,
                              nullptr);

    return eatString(clang_getFileName(file)) + ":" + QString::number(line) + ":" + QString::number(column);
}

inline static QString toString(CXSourceRange range)
{
    if (clang_equalRanges(range, clang_getNullRange()))
        return QString();

    QString ret = toString(clang_getRangeStart(range));

    unsigned line { 0 };
    unsigned column { 0 };
    clang_getSpellingLocation(clang_getRangeEnd(range),
                              nullptr,
                              &line,
                              &column,
                              nullptr);
    ret += " - " + QString::number(line) + ":" + QString::number(column);
    return ret;
}

inline static QString displayName(const CXType &type)
{
    assert(type.kind != CXType_Invalid);
    const QString ret = eatString(clang_getTypeSpelling(type));
    if (ret.isEmpty()) {
        qDebug() << "GOT AN EMPTY TYPE" << eatString(clang_getTypeKindSpelling(type.kind));
    }
    return ret;
}

inline static QString displayName(const CXCursor &cursor)
{
    assert(!clang_isInvalid(clang_getCursorKind(cursor)));
    QString ret = eatString(clang_getCursorDisplayName(cursor));
    if (!ret.isEmpty())
        return ret;
    ret = eatString(clang_getCursorSpelling(cursor));

    if (!ret.isEmpty())
        return ret;

    const CXCursorKind kind = clang_getCursorKind(cursor);
    switch (kind) {
    case CXCursor_CXXAccessSpecifier:
        switch (clang_getCXXAccessSpecifier(cursor)) {
        case CX_CXXInvalidAccessSpecifier: break;
        case CX_CXXPublic: ret = "public"; break;
        case CX_CXXProtected: ret = "protected"; break;
        case CX_CXXPrivate: ret = "private"; break;
        }
        break;
    case CXCursor_UnexposedStmt:
    case CXCursor_DeclStmt:
    case CXCursor_CompoundStmt: ret = "{ ... }"; break;
    case CXCursor_ReturnStmt: ret = "return"; break;
    case CXCursor_BreakStmt: ret = "break"; break;
    case CXCursor_ContinueStmt: ret = "continue"; break;
    case CXCursor_CaseStmt: ret = "case ..."; break;
    case CXCursor_ForStmt: ret = "for (...;...;...)"; break;
    case CXCursor_CXXForRangeStmt: ret = "for (... : ...)"; break;
    case CXCursor_DoStmt: ret = "do { ..."; break;
    case CXCursor_WhileStmt: ret = "while (...)"; break;
    case CXCursor_IfStmt: ret = "if (...)"; break;
    case CXCursor_SwitchStmt: ret = "switch (...)"; break;
    case CXCursor_DefaultStmt: ret = "default: ..."; break;
    case CXCursor_IndirectGotoStmt:
    case CXCursor_GotoStmt: ret = "goto ..."; break;
    case CXCursor_GCCAsmStmt: ret = "asm ..."; break;
    case CXCursor_NullStmt: ret = "null"; break;
    case CXCursor_EnumDecl: ret = "enum"; break;
    case CXCursor_UnionDecl: ret = "union"; break;
    case CXCursor_StructDecl: ret = "struct"; break;
    case CXCursor_ClassDecl: ret = "class"; break;
    case CXCursor_UnexposedDecl: ret = "unexposed"; break;
    case CXCursor_StaticAssert:
    case CXCursor_BinaryOperator:
    case CXCursor_IntegerLiteral:
    case CXCursor_FloatingLiteral:
    case CXCursor_ImaginaryLiteral:
    case CXCursor_CharacterLiteral: {
        CXToken *tokens = nullptr;
        unsigned numTokens = 0;
        CXTranslationUnit tu = clang_Cursor_getTranslationUnit(cursor);
        CXSourceRange range = clang_getCursorExtent(cursor);
        clang_tokenize(tu, range, &tokens, &numTokens);

        if (numTokens) {
            for (unsigned i=0; i<numTokens; ++i) {
                ret += eatString(clang_getTokenSpelling(tu, tokens[i]));
            }
        }
        clang_disposeTokens(tu, tokens, numTokens);
        break; }
    default:
        break;
    }
    if (ret.isEmpty()) {
        qDebug() << "GOT AN EMPTY ONE" << eatString(clang_getCursorKindSpelling(kind));
    }

    return ret;

}

inline static QString sourceCode(const CXCursor &cursor)
{
    const CXSourceRange range = clang_getCursorExtent(cursor);
    if (clang_equalRanges(range, clang_getNullRange()))
        return QString();

    CXFile file { nullptr };
    unsigned startOffset { 0 };
    unsigned endOffset { 0 };
    clang_getSpellingLocation(clang_getRangeStart(range),
                              &file,
                              nullptr,
                              nullptr,
                              &startOffset);
    clang_getSpellingLocation(clang_getRangeEnd(range),
                              nullptr,
                              nullptr,
                              nullptr,
                              &endOffset);

    if (startOffset >= endOffset)
        return "No source code";

    QFile f(eatString(clang_getFileName(file)));
    if (!f.open(QIODevice::ReadOnly))
        return "Couldn't open " + f.fileName() + " for reading";

    f.seek(startOffset);
    QString data = f.read(endOffset - startOffset);
    f.close();
    return data;
}

inline static QString toString(Node::NodeType type)
{
    switch (type) {
    case Node::Root: return "Root";
    case Node::Children: return "Children";
    case Node::Reference: return "Reference";
    case Node::SemanticParent: return "SemanticParent";
    case Node::LexicalParent: return "LexicalParent";
    case Node::Definition: return "Definition";
    case Node::Canonical: return "Canonical";
    case Node::SpecializedCursorTemplate: return "SpecializedCursorTemplate";
    case Node::Type: return "Type";
    case Node::TypedefUnderlyingType: return "TypedefUnderlyingType";
    case Node::EnumIntegerType: return "EnumIntegerType";
    case Node::CanonicalType: return "CanonicalType";
    case Node::PointeeType: return "PointeeType";
    case Node::Result: return "Result";
    case Node::ElementType: return "ElementType";
    case Node::TemplateArgument: return "TemplateArgument";
    case Node::Argument: return "Argument";
    case Node::ArgumentType: return "ArgumentType";
    case Node::OverloadedDeclaration: return "OverloadedDeclaration";
    case Node::TypeDeclaration: return "TypeDeclaration";
    }
    return QString();
}

inline static QString toNodeType(const Node *node)
{
    QString ret = toString(node->nodeType()) + " ";
    if (isValid(node->clangCursor())) {
        ret += "CXCursor";
    } else {
        assert(isValid(node->clangType()));
        ret += "CXType";
    }
    return ret;
}

Model::Model(std::unique_ptr<TranslationUnit> &&translationUnit, QObject *parent)
    : QAbstractItemModel(parent), mTranslationUnit(std::move(translationUnit))
{
}

QModelIndex Model::index(int row, int column,
                         const QModelIndex &parent) const
{
    Node *p = static_cast<Node *>(parent.internalPointer());
    if (!p) {
        if (!row)
            return createIndex(row, column, mTranslationUnit->root());
        return QModelIndex();
    }

    int r = 0;
    for (int i = Node::FirstBit; i<=Node::LastBit; ++i) {
        Node::NodeType nodeType = static_cast<Node::NodeType>(1ull << i);
        Node **child = nullptr;
        std::vector<Node *> *children = nullptr;
        p->extract(nodeType, &child, &children);
        if (child && *child) {
            if (r == row) {
                return createIndex(row, column, *child);
            }
            ++r;
        } else if (children) {
            if (row - r < static_cast<int>(children->size())) {
                return createIndex(row, column, (*children)[row - r]);
            } else {
                r += children->size();
            }
        }
    }
    return QModelIndex();
}

QModelIndex Model::parent(const QModelIndex &child) const
{
    Node *node = static_cast<Node *>(child.internalPointer());
    if (!node || !node->parent())
        return QModelIndex();
    return createIndex(node->parent()->data(), 0, node->parent());
}

int Model::rowCount(const QModelIndex &parent) const
{
    Node *node = static_cast<Node *>(parent.internalPointer());
    if (!node)
        return 1;
    size_t ret = 0;
    for (int i = Node::FirstBit; i<=Node::LastBit; ++i) {
        Node::NodeType nodeType = static_cast<Node::NodeType>(1ull << i);
        Node **child = nullptr;
        std::vector<Node *> *children = nullptr;
        node->extract(nodeType, &child, &children);
        if (child && *child) {
            ++ret;
        } else if (children) {
            ret += children->size();
        }
    }
    return ret;
}

int Model::columnCount(const QModelIndex &/*parent*/) const
{
    return ColumnCount;
}

bool Model::hasChildren(const QModelIndex &parent) const
{
    Node *node = static_cast<Node *>(parent.internalPointer());
    if (!node)
        return true;

    for (int i = Node::FirstBit; i<=Node::LastBit; ++i) {
        Node::NodeType nodeType = static_cast<Node::NodeType>(1ull << i);
        Node **child = nullptr;
        std::vector<Node *> *children = nullptr;
        node->extract(nodeType, &child, &children);
        if ((child && *child) || (children && children->size()))
            return true;
    }
    return false;
}

QVariant Model::data(const QModelIndex &index, int role) const
{
    Node *node = static_cast<Node *>(index.internalPointer());
    if (!node)
        return QVariant();

    const CXCursor &cursor = node->clangCursor();
    const CXType &type = node->clangType();
    switch (role) {
    case Qt::DisplayRole:
        switch (index.column()) {
        case DisplayName:
            if (type.kind != CXType_Invalid) {
                return displayName(type);
            }
            return displayName(cursor);
        case NodeType: return toNodeType(node);
        case Kind:
            if (type.kind != CXType_Invalid)
                return eatString(clang_getTypeKindSpelling(type.kind));
            return eatString(clang_getCursorKindSpelling(clang_getCursorKind(cursor)));
        case Location: return toString(clang_getCursorExtent(cursor));
        }
        break;
    case Model::SourceCodeRole:
        return sourceCode(cursor);
    case Qt::ToolTipRole:
        return data(index, Qt::DisplayRole).value<QString>() + " " + data(index, Model::SourceCodeRole).value<QString>();
    case Qt::DecorationRole:
    case Qt::EditRole:
    case Qt::StatusTipRole:
    case Qt::WhatsThisRole:
    case Qt::SizeHintRole:
        break;
    }
    return QVariant();
}

QVariant Model::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (role != Qt::DisplayRole || orientation != Qt::Horizontal)
        return QVariant();

    switch (section) {
    case DisplayName: return "DisplayName";
    case NodeType: return "Type";
    case Kind: return "Kind";
    case Location: return "Location";
    }
    return QVariant();
}

unsigned int Model::translationUnitFlags() const
{
    return mTranslationUnit->flags();
}

void Model::setTranslationUnitFlags(unsigned int flags)
{
    if (flags != mTranslationUnit->flags()) {
        beginResetModel();
        mTranslationUnit->setFlags(flags);
        endResetModel();
    }
}
