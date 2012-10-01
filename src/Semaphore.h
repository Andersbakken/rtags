#ifndef SEMAPHORE_H
#define SEMAPHORE_H

#include "Path.h"

class Semaphore
{
public:
    Semaphore(int key, int value = 1);
    Semaphore(const Path& filename, int value = 1);
    ~Semaphore();

    void acquire(int num = 1);
    void release(int nul = 1);

    void op(int value);

    bool isValid() const { return mSem != -1; }

private:
    int mSem;
    bool mOwner;
};

#endif
