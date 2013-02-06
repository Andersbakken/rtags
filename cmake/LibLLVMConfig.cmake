find_path(LibLLVM_INCLUDE_DIR llvm/Support/Casting.h)

find_library(LibLLVM_LIBRARY NAMES llvm)

set(LibLLVM_LIBRARIES ${LibLLVM_LIBRARY})
set(LibLLVM_INCLUDE_DIRS ${LibLLVM_INCLUDE_DIR})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LibLLVM DEFAULT_MSG LibLLVM_LIBRARY LibLLVM_INCLUDE_DIR)

mark_as_advanced(LibLLVM_INCLUDE_DIR LibLLVM_LIBRARY)
