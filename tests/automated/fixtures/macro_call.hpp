#include <cerrno>

#define EINTRWRAP(VAR, BLOCK) \
    do {                      \
        VAR = BLOCK;          \
    } while (VAR == -1 && errno == EINTR)
