cmake_minimum_required(VERSION 2.8.6)
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

include(CheckSymbolExists)
include(CheckCXXSymbolExists)

if(CMAKE_BUILD_TYPE MATCHES "Debug")
  add_definitions(-DRTAGS_DEBUG)
endif()

add_definitions(-Wall "-DCLANG_INCLUDEPATH=\"${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/clang_install/lib/clang/3.1/include/\""
  "-DCLANG_BIN=\"${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/clang_install/bin/\"")

add_definitions(-DOS_${CMAKE_SYSTEM_NAME})

if(CMAKE_SYSTEM_NAME MATCHES "Linux")
  set(system_LIBS rt)
elseif(CMAKE_SYSTEM_NAME MATCHES "FreeBSD")
  add_definitions("-DMAKE=\"gmake\"")
endif()

check_cxx_symbol_exists(backtrace "execinfo.h" HAVE_BACKTRACE)
check_cxx_symbol_exists(CLOCK_MONOTONIC_RAW "time.h" HAVE_CLOCK_MONOTONIC_RAW)
check_cxx_symbol_exists(CLOCK_MONOTONIC "time.h" HAVE_CLOCK_MONOTONIC)
check_cxx_symbol_exists(mach_absolute_time "mach/mach.h;mach/mach_time.h" HAVE_MACH_ABSOLUTE_TIME)
check_cxx_symbol_exists(inotify_init "sys/inotify.h" HAVE_INOTIFY)
check_cxx_symbol_exists(kqueue "sys/types.h;sys/event.h" HAVE_KQUEUE)
check_cxx_symbol_exists(SO_NOSIGPIPE "sys/types.h;sys/socket.h" HAVE_NOSIGPIPE)
check_cxx_symbol_exists(MSG_NOSIGNAL "sys/types.h;sys/socket.h" HAVE_NOSIGNAL)
check_cxx_symbol_exists(SA_SIGINFO "signal.h" HAVE_SIGINFO)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.h.in
               ${CMAKE_CURRENT_BINARY_DIR}/config.h)

include(rtags.cmake)

include_directories(${CMAKE_CURRENT_LIST_DIR})
add_executable(rdm rdm.cpp)
target_link_libraries(rdm rtags ${clang_LIBS} ${system_LIBS})
add_dependencies(rdm rtags)

add_executable(rc rc.cpp)
target_link_libraries(rc rtags ${clang_LIBS} ${system_LIBS})
add_dependencies(rc rtags)
