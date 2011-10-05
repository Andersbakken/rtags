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

Configuration *loadConfiguration(const char *path)
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
    Configuration *conf = malloc(sizeof(Configuration));
    memset(conf, 0, sizeof(Configuration));
    conf->buffer = malloc(st.st_size + 1);
    if (fread(conf->buffer, sizeof(char), st.st_size, f) != (size_t)st.st_size) {
        fprintf(stderr, "%s %d: if (fread(file, sizeof(char), st.st_size, f) != st.st_size) {\n", __FILE__, __LINE__);
        free(conf->buffer);
        free(conf);
        fclose(f);
        return 0;
    }
    conf->buffer[st.st_size] = '\0';
    fclose(f);

    conf->count = 0;
    int first = 1;
    while (1) {
        char *line = strtok(first ? conf->buffer : 0, "\n");
        first = 0;
        if (!line)
            break;
        ++conf->count;
        conf->keys = realloc(conf->keys, sizeof(const char*) * conf->count);
        conf->values = realloc(conf->values, sizeof(const char*) * conf->count);
        conf->keys[conf->count - 1] = line;
        char *colon = strchr(line, ':');
        conf->values[conf->count - 1] = colon ? colon + 1 : 0;
        if (colon)
            *colon = '\0';
    }
    return conf;
}
const char *configurationKey(Configuration *conf, int idx)
{
    return (conf && idx >= 0 && idx < conf->count ? conf->keys[idx] : 0);
}

const char *configurationValue(Configuration *conf, int idx)
{
    return (conf && idx >= 0 && idx < conf->count ? conf->values[idx] : 0);
}

int configurationCount(Configuration *conf)
{
    return conf ? conf->count : 0;
}

void unloadConfiguration(Configuration *conf)
{
    if (conf) {
        if (conf->values)
            free(conf->values);
        if (conf->keys)
            free(conf->keys);
        if (conf->buffer)
            free(conf->buffer);
        free(conf);
    }
}
