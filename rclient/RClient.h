#ifndef RClient_h
#define RClient_h

#include <Shared.h>
typedef enum {
    CaseInsensitive = 0x1
} Flag;

struct NodeData readNode(int32_t address);
void initRClient(struct MMapData *mmapData, unsigned flags);
int findByLocation(const char *location, struct NodeData *nodeData);
int findSibling(const struct NodeData *nodeData, struct NodeData *sibling, int type);

typedef struct {
    char *buffer;
    int count;
    const char **keys;
    const char **values;
} Configuration;
Configuration *loadConfiguration(const char *file);
const char *configurationKey(Configuration *conf, int idx);
const char *configurationValue(Configuration *conf, int idx);
int configurationCount(Configuration *conf);
void unloadConfiguration(Configuration *conf);

#endif
