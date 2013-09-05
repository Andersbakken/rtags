#ifndef CONFIG_H
#define CONFIG_H

/* #undef HAVE_BACKTRACE */
/* #undef HAVE_CLOCK_MONOTONIC_RAW */
#define HAVE_CLOCK_MONOTONIC
/* #undef HAVE_MACH_ABSOLUTE_TIME */
/* #undef HAVE_INOTIFY */
/* #undef HAVE_KQUEUE */
#define HAVE_FAM
/* #undef HAVE_EPOLL */
/* #undef HAVE_NOSIGPIPE */
#define HAVE_NOSIGNAL
#define HAVE_SIGINFO
/* #undef HAVE_FSEVENTS */
#define HAVE_STATMTIM

#endif
