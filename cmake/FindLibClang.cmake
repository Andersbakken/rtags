if(NOT DEFINED CLANG_ROOT)
  set(CLANG_ROOT $ENV{CLANG_ROOT})
endif()

find_path(LibClang_INCLUDE_DIR clang-c/Index.h HINTS "${CLANG_ROOT}/include")
find_library(LibClang_LIBRARY NAMES clang HINTS "${CLANG_ROOT}/lib")

set(LibClang_LIBRARIES ${LibClang_LIBRARY})
set(LibClang_INCLUDE_DIRS ${LibClang_INCLUDE_DIR})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LibClang DEFAULT_MSG LibClang_LIBRARY LibClang_INCLUDE_DIR)

mark_as_advanced(LibClang_INCLUDE_DIR LibClang_LIBRARY)
