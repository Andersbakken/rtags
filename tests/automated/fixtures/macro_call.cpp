struct sockaddr
{
};

struct SocketAddress
{
    const struct sockaddr *sockaddr() const;
};

int connect(int fd, const struct sockaddr *addr, unsigned long len);

#include "macro_call.hpp"

int connectSocket(SocketAddress sockAddr)
{
    int ret;
    sockAddr.sockaddr();
    EINTRWRAP(ret, connect(0, sockAddr.sockaddr(), 0));
    return ret;
}
