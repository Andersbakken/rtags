#ifndef SHAREDMEMORY_H
#define SHAREDMEMORY_H

#include "Path.h"

class SharedMemory
{
public:
    enum Flags { Read = 0x0, Write = 0x1, ReadWrite = Write };

    SharedMemory(int key, unsigned int size);
    SharedMemory(const Path& filename, unsigned int size);
    ~SharedMemory();

    void* attach(Flags flags, void* address = 0);
    void detach();

    bool isValid() const { return mShm != -1; }

private:
    int mShm;
    bool mOwner;
    void* mAddr;
};

#endif
