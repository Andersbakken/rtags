#include "RClient.h"
#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/mman.h>

static struct MMapData *sDB = 0;
static unsigned sFlags = 0;
void initRClient(struct MMapData *mmapData, unsigned flags)
{
    sFlags = flags;
    sDB = mmapData;
}

struct NodeData readNode(int32_t address)
{
    assert(sDB);
    return readNodeData(sDB->memory + address);
}

static int find(const void *l, const void *r)
{
    assert(sDB);
    const char *left = ((const char*)l) + Int32Length;
    const char *right = ((const char*)r) + Int32Length;
    // printf("%s %s %s\n", left, right, std::string(left, idLength).c_str());
    return ((sFlags & CaseInsensitive)
            ? strncasecmp(left, right, sDB->idLength - Int32Length)
            : strncmp(left, right, sDB->idLength - Int32Length));
}

int findByLocation(const char *location, struct NodeData *nodeData)
{
    assert(location);
    const int locationLen = strlen(location) + 1;
    char *padded = (char*)malloc(locationLen + Int32Length);
    strncpy(padded + Int32Length, location, locationLen);
    const char *bs = (const char*)bsearch(padded, sDB->memory + FirstId, sDB->nodeCount, sDB->idLength, find);
    // fprintf(stderr, "Found a match %p\n", bs);
    free(padded);
    if (!bs)
        return 0;

    const int32_t idx = readInt32(bs);
    *nodeData = readNodeData(sDB->memory + idx);
    return 1;
}

int findSibling(const struct NodeData *nodeData, struct NodeData *sibling, int type)
{
    assert(sDB);
    const struct NodeData parent = readNode(nodeData->parent);
    assert(parent.firstChild);
    *sibling = readNode(parent.firstChild);
    while (1) {
        if (sibling->type == type && !strcmp(nodeData->symbolName, sibling->symbolName)) {
            return 1;
        }
        if (!sibling->nextSibling)
            break;
        *sibling = readNode(sibling->nextSibling);
    }
    return 0;
}

const char *strnstr(const char *haystack, const char *needle, int len)
{
    assert(haystack);
    assert(needle);
    if (!len)
        return 0;
    const int needleLen = strlen(needle);
    assert(needleLen);

    int state = 0;
    int i = 0;
    while (i < len) {
        if (haystack[i] == needle[state]) {
            ++state;
            if (state == needleLen)
                return haystack + (i - needleLen);
        } else if (state > 0) {
            state = 0;
            continue;
        }
        ++i;
    }
    return 0;

}

int loadConfiguration(const char *path, const char *key, char *buf, int bufLen, int idx)
{
    struct stat st;

    if (stat(path, &st) < 0) {
        fprintf(stderr, "%s %d: if (stat(path, &st) < 0) {\n", __FILE__, __LINE__);
        return 0;
    }
    FILE *f = fopen(path, "r");
    if (!f) {
        fprintf(stderr, "%s %d: if (f) {\n", __FILE__, __LINE__);
        return 0;
    }
    
    char *fileData = malloc(st.st_size + 1);
    if (fread(fileData, sizeof(char), st.st_size, f) != (size_t)st.st_size) {
        fprintf(stderr, "%s %d: if (fread(file, sizeof(char), st.st_size, f) != st.st_size) {\n", __FILE__, __LINE__);
        free(fileData);
        fclose(f);
        return 0;
    }
    fileData[st.st_size] = '\0';
    fclose(f);

    assert(key);
    const int keyLen = strlen(key);
    assert(keyLen);
    
    int found = 0;
    int first = 1;
    do {
        char *line = strtok(first ? fileData : 0, "\n");
        first = 0;
        if (!line)
            break;

        const int lineLen = strlen(line);
        if (lineLen - keyLen > 2 && !strncmp(line, key, keyLen)
            && line[keyLen] == ':' && !idx--) {
            int valueLen = lineLen - keyLen - 1;
            if (valueLen > bufLen - 1)
                valueLen = bufLen - 1;
            strncpy(buf, line + keyLen + 1, valueLen);
            buf[valueLen] = '\0';
            found = 1;
        }
    } while (!found);
    free(fileData);
    return found;
}
