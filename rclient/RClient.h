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

#endif
