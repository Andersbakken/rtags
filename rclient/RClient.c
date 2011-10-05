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

int loadConfigFile(const char *path, char **args)
{
    struct stat st;
    const int fd = open(path, O_RDONLY);
    if (fd <= 0) {
        printf("%s %d: if (fd <= 0)\n", __FILE__, __LINE__);
        return -1;
    }

    if (fstat(fd, &st) < 0) {
        close(fd);
        printf("%s %d: if (fstat(fdin, &st) < 0) \n", __FILE__, __LINE__);
        return -1;
    }
    const void *memory = mmap(0, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
    if (memory == MAP_FAILED) {
        close(fd);
    }
    const char *ch = (const char*)memory;
    int pos = 0;
    int last = 0;
    int lines = 0;
    while (pos < st.st_size) {
        if (ch[pos] == '\n') {
            if (pos - last > 1)
                ++lines;
            last = pos;
        }
        ++pos;
    }
    args = malloc(sizeof(char*) * ((2 * lines) + 1)); // use realloc instead of running through twice
    args[lines] = 0;
    int colon = -1;
    int line = 0;
    while (pos < st.st_size) {
        switch (ch[pos]) {
        case '\n': {
            const int len = (pos - last - 1);
            if (len > 0) {
                args[(line * 2)] = strndup(ch + last, colon == -1 ? len : colon - last);
                /* if (colon */
            }

            last = pos;
            break; }
        case ':':
            if (colon == -1)
                colon = pos;
            break;
        default:
            break;
        }
        ++pos;
    }
    
    return 0;
}

