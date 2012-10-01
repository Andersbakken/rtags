#include "SharedMemory.h"
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <assert.h>
#include <errno.h>

#define PROJID 3946

SharedMemory::SharedMemory(int key, unsigned int size)
    : mAddr(0)
{
    int flg = IPC_CREAT | IPC_EXCL;
    do {
        mShm = shmget(key, size, flg);
        if (mShm == -1) {
            if (flg && errno == EEXIST)
                flg = 0;
            else if (!flg && errno == ENOENT)
                flg = IPC_CREAT | IPC_EXCL;
            else
                break;
        }
    } while (mShm == -1);
    mOwner = ((flg & IPC_CREAT) == IPC_CREAT);
}

SharedMemory::SharedMemory(const Path& filename, unsigned int size)
    : mShm(-1), mOwner(false), mAddr(0)
{
    const key_t key = ftok(filename.nullTerminated(), PROJID);
    if (key == -1)
        return;
    int flg = IPC_CREAT | IPC_EXCL;
    do {
        mShm = shmget(key, size, flg);
        if (mShm == -1) {
            if (flg && errno == EEXIST)
                flg = 0;
            else if (!flg && errno == ENOENT)
                flg = IPC_CREAT | IPC_EXCL;
            else
                break;
        }
    } while (mShm == -1);
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

void* SharedMemory::attach(Flags flags, void* address)
{
    int flg = SHM_RND;
    if (!(flags & Write))
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
