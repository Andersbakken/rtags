set(clang_ROOT $ENV{CLANG_ROOT})
if("${clang_ROOT}" STREQUAL "")
  set(clang_ROOT "${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/clang_install/")
endif()

get_filename_component(PARENT_DIR ${CMAKE_CURRENT_LIST_DIR} PATH)

include_directories(${clang_ROOT}/include)
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(clang_LIBS
    ${clang_ROOT}/lib/liblibclang.a
    ${clang_ROOT}/lib/libclangBasic.a
    ${clang_ROOT}/lib/libclangLex.a
    ${clang_ROOT}/lib/libclangParse.a
    ${clang_ROOT}/lib/libclangAST.a
    ${clang_ROOT}/lib/libclangfrontend.a
    ${clang_ROOT}/lib/libLLVMSupport.a
    ${clang_ROOT}/lib/libclangsema.a
    ${clang_ROOT}/lib/libLLVMMC.a
    ${clang_ROOT}/lib/libclangSerialization.a
    ${clang_ROOT}/lib/libclangDriver.a
    ${clang_ROOT}/lib/libclangEdit.a
    ${clang_ROOT}/lib/libclangAnalysis.a
    ${PARENT_DIR}/3rdparty/leveldb/libleveldb.a crypto)
else()
    set(clang_LIBS ${clang_ROOT}/lib/liblibclang.so ${PARENT_DIR}/3rdparty/leveldb/libleveldb.a crypto)
endif()
