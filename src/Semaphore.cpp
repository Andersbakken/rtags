#include "Semaphore.h"
#include "RTags.h"
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <errno.h>

#define PROJID 3945

Semaphore::Semaphore(int key, CreateFlag flag, int value)
{
    const int flg = (flag == Create) ? (IPC_CREAT | IPC_EXCL) : 0;
    mSem = semget(key, value, flg);
    mOwner = ((flg & IPC_CREAT) == IPC_CREAT);
}

Semaphore::Semaphore(const Path& filename, CreateFlag flag, int value)
    : mSem(-1), mOwner(false)
{
    const key_t key = ftok(filename.nullTerminated(), PROJID);
    if (key == -1)
        return;
    const int flg = (flag == Create) ? (IPC_CREAT | IPC_EXCL) : 0;
    mSem = semget(key, value, flg);
    mOwner = ((flg & IPC_CREAT) == IPC_CREAT);
}

Semaphore::~Semaphore()
{
    if (mSem != -1 && mOwner)
        semctl(mSem, 0, IPC_RMID, 0);
}

void Semaphore::acquire(int num)
{
    sembuf buf = { 0, num * -1, SEM_UNDO };
    int ret;
    eintrwrap(ret, semop(mSem, &buf, 1));
    if (ret == -1 && errno == EIDRM)
        mSem = -1;
}

void Semaphore::release(int num)
{
    sembuf buf = { 0, num, SEM_UNDO };
    int ret;
    eintrwrap(ret, semop(mSem, &buf, 1));
    if (ret == -1 && errno == EIDRM)
        mSem = -1;
}

void Semaphore::op(int num)
{
    sembuf buf = { 0, num, SEM_UNDO };
    int ret;
    eintrwrap(ret, semop(mSem, &buf, 1));
    if (ret == -1 && errno == EIDRM)
        mSem = -1;
}
