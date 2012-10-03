#ifndef SEMAPHORE_H
#define SEMAPHORE_H

#include "Path.h"

class Semaphore
{
public:
    enum CreateFlag { None, Create };

    Semaphore(int key, CreateFlag flag = None, int value = 1);
    Semaphore(const Path& filename, CreateFlag flag = None, int value = 1);
    ~Semaphore();

    void acquire(short num = 1);
    void release(short nul = 1);

    void op(short value);

    bool isValid() const { return mSem != -1; }

private:
    int mSem;
    bool mOwner;
};

#endif
