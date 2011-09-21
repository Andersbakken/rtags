#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <QtCore>

qint32 locationLength = -1;

static int find(const void *left, const void *right)
{
    printf("%s %s\n",
           reinterpret_cast<const char*>(left),
           reinterpret_cast<const char*>(right));
    return strncmp(reinterpret_cast<const char*>(left),
                   reinterpret_cast<const char*>(right),
                   locationLength);
}

static inline void readNode(const char *base, quint8 *type, qint32 *location, qint32 *parent,
                            qint32 *nextSibling, qint32 *firstChild, const char **symbolName)
{
    if (type)
        memcpy(type, base, sizeof(quint8));
    if (location)
        memcpy(location, base + sizeof(quint8), sizeof(qint32));
    if (parent)
        memcpy(parent, base + sizeof(quint8) + sizeof(qint32), sizeof(qint32));
    if (nextSibling)
        memcpy(nextSibling, base + sizeof(quint8) + (sizeof(qint32) * 2), sizeof(qint32));
    if (firstChild)
        memcpy(firstChild, base + sizeof(quint8) + (sizeof(qint32) * 3), sizeof(qint32));
    if (symbolName)
        *symbolName = reinterpret_cast<const char*>(base + sizeof(quint8) + (sizeof(qint32) * 4));
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

    for (int i=0; i<12; ++i) {
        printf("%d %x %c\n", i, ch[i], ch[i]);
    }

    qint32 nodeCount = -1;
    memcpy(&nodeCount, ch + 2, sizeof(qint32));
    memcpy(&locationLength, ch + 2 + sizeof(qint32), sizeof(qint32));
    // printf("%d %d\n", locationLength, nodeCount);
    // qDebug() << (locationLength + 1 + sizeof(qint32));
    if (locationLength > 0) {
        const void *bs = bsearch(argv[2],
                                 ch + (sizeof(qint32) * 2) + 2,
                                 nodeCount,
                                 locationLength + 1 + sizeof(qint32),
                                 find);
        if (bs) {
            const char *symbolName;
            quint8 type;
            qint32 parent;
            readNode(reinterpret_cast<const char*>(bs), &type, 0, &parent, 0, 0, &symbolName);
            printf("Found %p %s %d %d\n", bs, symbolName, type, parent);
        }
    }

    munmap(mapped, st.st_size);
    close(fd);
    return 0;
}
