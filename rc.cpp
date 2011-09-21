#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include "NodeType.h"

int32_t locationLength = -1;

static int find(const void *l, const void *r)
{
    const char *left = reinterpret_cast<const char*>(l) + sizeof(int32_t);
    const char *right = reinterpret_cast<const char*>(r) + sizeof(int32_t);
    // printf("%s %s\n", left, right);
    return strncmp(reinterpret_cast<const char*>(left),
                   reinterpret_cast<const char*>(right),
                   locationLength);
}

static inline void readNode(const char *base, int32_t *type, int32_t *location, int32_t *parent,
                            int32_t *nextSibling, int32_t *firstChild, const char **symbolName)
{
    if (type)
        memcpy(type, base, sizeof(int32_t));
    if (location)
        memcpy(location, base + sizeof(int32_t), sizeof(int32_t));
    if (parent)
        memcpy(parent, base + sizeof(int32_t) + sizeof(int32_t), sizeof(int32_t));
    if (nextSibling)
        memcpy(nextSibling, base + sizeof(int32_t) + (sizeof(int32_t) * 2), sizeof(int32_t));
    if (firstChild)
        memcpy(firstChild, base + sizeof(int32_t) + (sizeof(int32_t) * 3), sizeof(int32_t));
    if (symbolName)
        *symbolName = reinterpret_cast<const char*>(base + sizeof(int32_t) + (sizeof(int32_t) * 4));
}

int main(int argc, char **argv)
{
    int fd;
    struct stat st;
    void *mapped = 0;
    const char *ch = 0;

    if (argc < 3) {
        printf("%s %d: if (argc < 3) {\n", __FILE__, __LINE__);
        return 1;
    }

    fd = open(argv[1], O_RDONLY);
    if (fd <= 0) {
        printf("%s %d: if (fd <= 0)\n", __FILE__, __LINE__);
        return 1;
    }

    if (fstat(fd, &st) < 0) {
        printf("%s %d: if (fstat(fdin, &st) < 0) \n", __FILE__, __LINE__);
        close(fd);
        return 1;
    }
    if (st.st_size < 10) {
        printf("%s %d: if (st.st_size < 10) {\n", __FILE__, __LINE__);
        return 1;
    }
        
    if ((mapped = mmap(0, st.st_size, PROT_READ, MAP_SHARED, fd, 0)) == MAP_FAILED) {
        printf("%s %d: if ((src = mmap(0, st.st_size, PROT_READ, MAP_SHARED, fd, 0)) == MAP_FAILED) {\n", __FILE__, __LINE__);
        close(fd);
        return 1;
    }
    ch = reinterpret_cast<char*>(mapped);
    if (memcmp(mapped, "Rt", 2)) {
        printf("%s %d: if (memcmp(mapped, \"Rt\", 2)) {\n", __FILE__, __LINE__);
        munmap(mapped, st.st_size);
        close(fd);
        return 1;
    }

    for (int i=0; i<10; ++i) {
        printf("%d %x %c\n", i, ch[i], ch[i]);
    }

    int32_t nodeCount = -1;
    memcpy(&nodeCount, ch + 2, sizeof(int32_t));
    memcpy(&locationLength, ch + 2 + sizeof(int32_t), sizeof(int32_t));
    // printf("%d %d\n", locationLength, nodeCount);
    // qDebug() << (locationLength + 1 + sizeof(int32_t));
    if (locationLength > 0) {
        const int argv2Len = strlen(argv[2]);
        char *arg = new char[argv2Len + sizeof(int32_t)];
        strncpy(arg + sizeof(int32_t), argv[2], argv2Len);
        const void *bs = bsearch(arg,
                                 ch + (sizeof(int32_t) * 2) + 2,
                                 nodeCount,
                                 locationLength + 1 + sizeof(int32_t),
                                 find);
        delete []arg;
        if (bs) {
            int32_t idx = *reinterpret_cast<const int32_t*>(bs);
            const char *symbolName;
            int32_t type;
            int32_t parent;
            readNode(ch + idx, &type, 0, &parent, 0, 0, &symbolName);
            printf("Found %s %d %d\n", symbolName, type, parent);
        }
    }

    munmap(mapped, st.st_size);
    close(fd);
    return 0;
}
