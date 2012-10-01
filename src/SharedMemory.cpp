#include "SharedMemory.h"
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <assert.h>
#include <errno.h>

#define PROJID 3946

SharedMemory::SharedMemory(int key, unsigned int size, CreateFlag flag)
    : mAddr(0)
{
    const int flg = (flag == Create) ? (IPC_CREAT | IPC_EXCL) : 0;
    mShm = shmget(key, size, flg);
    mOwner = ((flg & IPC_CREAT) == IPC_CREAT);
}

SharedMemory::SharedMemory(const Path& filename, unsigned int size, CreateFlag flag)
    : mShm(-1), mOwner(false), mAddr(0)
{
    const key_t key = ftok(filename.nullTerminated(), PROJID);
    if (key == -1)
        return;
    const int flg = (flag == Create) ? (IPC_CREAT | IPC_EXCL) : 0;
    mShm = shmget(key, size, flg);
    mOwner = ((flg & IPC_CREAT) == IPC_CREAT);
}

SharedMemory::~SharedMemory()
{
    if (mAddr) {
        assert(mShm != -1);
        shmdt(mAddr);
    }
    if (mShm != -1 && mOwner)
        shmctl(mShm, IPC_RMID, 0);
}

void* SharedMemory::attach(AttachFlag flag, void* address)
{
    int flg = SHM_RND;
    if (!(flag & Write))
        flg |= SHM_RDONLY;
    mAddr = shmat(mShm, address, flg);
    return mAddr;
}

void SharedMemory::detach()
{
    if (!mAddr)
        return;
    shmdt(mAddr);
    mAddr = 0;
}
