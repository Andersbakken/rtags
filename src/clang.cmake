set(clang_ROOT $ENV{CLANG_ROOT})
if("${clang_ROOT}" STREQUAL "")
  set(clang_ROOT "${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/clang_install/")
endif()

get_filename_component(PARENT_DIR ${CMAKE_CURRENT_LIST_DIR} PATH)

SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

include_directories(${clang_ROOT}/include)
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(clang_LIBS ${clang_ROOT}/lib/libclang.dylib)
else()
    set(clang_LIBS ${clang_ROOT}/lib/libclang.so)
endif()
