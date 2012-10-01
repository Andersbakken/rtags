#include "Semaphore.h"
#include "RTags.h"
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <errno.h>

#define PROJID 3945

Semaphore::Semaphore(int key, int value)
{
    int flg = IPC_CREAT | IPC_EXCL;
    do {
        mSem = semget(key, value, flg);
        if (mSem == -1) {
            if (flg && errno == EEXIST)
                flg = 0;
            else if (!flg && errno == ENOENT)
                flg = IPC_CREAT | IPC_EXCL;
            else
                break;
        }
    } while (mSem == -1);
    mOwner = ((flg & IPC_CREAT) == IPC_CREAT);
}

Semaphore::Semaphore(const Path& filename, int value)
    : mSem(-1)
{
    key_t key = ftok(filename.nullTerminated(), PROJID);
    if (key == -1)
        return;
    int flg = IPC_CREAT | IPC_EXCL;
    do {
        mSem = semget(key, value, flg);
        if (mSem == -1) {
            if (flg && errno == EEXIST)
                flg = 0;
            else if (!flg && errno == ENOENT)
                flg = IPC_CREAT | IPC_EXCL;
            else
                break;
        }
    } while (mSem == -1);
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
