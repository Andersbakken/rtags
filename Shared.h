#ifndef Shared_h
#define Shared_h

#include <stdint.h>
#include <string.h>

#ifdef __cplusplus
#ifdef QT
#include <QByteArray>
#include <QIODevice>
#endif
extern "C" {
#endif

typedef enum {
    Int32Length = 4,
    MagicPos = 0,
    MagicLength = 3,
    NodeCountLength = Int32Length,
    NodeCountPos = (MagicPos + MagicLength),
    IdLengthLength = Int32Length,
    IdLengthPos = (NodeCountPos + NodeCountLength),
    DictionaryPosPos = IdLengthPos + IdLengthLength,
    DictionaryPosLength = Int32Length,
    DictionaryCountPos = DictionaryPosPos + DictionaryPosLength,
    DictionaryCountPosLength = Int32Length,
    DictionarySymbolNameLengthPos = DictionaryCountPos + DictionaryCountPosLength,
    DictionarySymbolNameLengthPosLength = Int32Length,
    DictionaryMaxSynonymsPos = DictionarySymbolNameLengthPos + DictionarySymbolNameLengthPosLength,
    DictionaryMaxSynonymsPosLength = Int32Length,
    FirstId = DictionaryMaxSynonymsPosLength + DictionaryMaxSynonymsPosLength,
    HeaderSize = FirstId
} Offset;

static inline int32_t rootNodePosition(int nodeCount, int idLength)
{
    return HeaderSize + (nodeCount * idLength);
}

static inline char *writeInt32(char *dest, int32_t value)
{
    memcpy(dest, &value, Int32Length);
    return dest + Int32Length;
}

static inline char *writeString(char *dest, const char *src, int len) // null-terminated
{
    if (len == -1)
        len = strlen(src) + 1;
    memcpy(dest, src, len);
    return dest + len;
}

static inline int32_t readInt32(const char *src)
{
    int32_t ret;
    memcpy(&ret, src, Int32Length);
    return ret;
}

struct NodeData {
    int32_t type, location, parent, nextSibling, firstChild;
    const char *symbolName;
};

static inline struct NodeData readNodeData(const char *base)
{
    struct NodeData n;
    int i;
    int32_t *ints[] = { &n.type, &n.location, &n.parent, &n.nextSibling, &n.firstChild, 0 };
    for (i=0; ints[i]; ++i) {
        *ints[i] = readInt32(base);
        base += Int32Length;
    }
    n.symbolName = base;
    return n;
}

typedef enum {
    Invalid = 0x000000,
    Root = 0x000001,
    Namespace = 0x000002,
    Class = 0x000004,
    Struct = 0x000008,
    MethodDefinition = 0x000010,
    MethodDeclaration = 0x000020,
    Variable = 0x000040,
    Enum = 0x000080,
    EnumValue = 0x000100,
    Typedef = 0x000200,
    MacroDefinition = 0x000400,
    Reference = 0x000800,
    // update stringToNodeType when adding types
    All = 0xffffff
} NodeType;
typedef enum {
    Abbreviated,
    Normal
} NodeTypeToNameMode;
const char *nodeTypeToName(int type, NodeTypeToNameMode mode);
NodeType stringToNodeType(const char *in);

#ifdef __cplusplus
}
#ifdef QT
static inline void writeInt32(QIODevice *dev, int32_t value)
{
    dev->write(reinterpret_cast<const char *>(&value), Int32Length);
}

static inline char *writeString(char *dest, const QByteArray &data)
{
    return writeString(dest, data.constData(), data.length() + 1);
}

static inline void writeString(QIODevice *dev, const char *src, int len = -1) // null-terminated
{
    if (len == -1)
        len = strlen(src) + 1;
    dev->write(src, len);
}

static inline void writeString(QIODevice *dev, const QByteArray &data)
{
    writeString(dev, data.constData(), data.length() + 1);
}
#endif
#endif
#endif
