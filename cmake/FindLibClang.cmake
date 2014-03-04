if(NOT DEFINED CLANG_ROOT)
  set(CLANG_ROOT $ENV{CLANG_ROOT})
endif()

find_path(CLANG_INCLUDE clang-c/Index.h
          HINTS
          ${CLANG_ROOT}/include
          /usr/lib/llvm-3.4/include
          /usr/lib/llvm-3.3/include
          /usr/lib/llvm-3.2/include
          /usr/lib/llvm-3.1/include
          /opt/local/libexec/llvm-3.4/include
          /opt/local/libexec/llvm-3.3/include
          /opt/local/libexec/llvm-3.2/include
          /opt/local/libexec/llvm-3.1/include
          /usr/lib/llvm-3.4/include
          NO_DEFAULT_PATH)
if (NOT EXISTS ${CLANG_INCLUDE})
  find_path(CLANG_INCLUDE clang-c/Index.h)
endif ()

find_path(CLANG_COMPILATION_INCLUDE clang-c/CXCompilationDatabase.h
	  HINTS
	  ${CLANG_ROOT}/include
	  /opt/local/libexec/llvm-3.4/include
	  /opt/local/libexec/llvm-3.3/include
	  /opt/local/libexec/llvm-3.2/include
	  /opt/local/libexec/llvm-3.1/include
	  /opt/local/libexec/llvm-3.0/include
	  /usr/lib/llvm-3.4/include
	  NO_DEFAULT_PATH)
if (NOT EXISTS ${CLANG_COMPILATION_INCLUDE})
  find_path(CLANG_COMPILATION_INCLUDE clang-c/CXCompilationDatabase.h)
endif ()

if (EXISTS ${CLANG_INCLUDE})
  if ("${CLANG_ROOT}" STREQUAL "")
	string(REGEX REPLACE "\\/include" "" CLANG_ROOT ${CLANG_INCLUDE})
  endif()
endif()
if (EXISTS "${CLANG_ROOT}/lib/libclang.so")
  set(CLANG_LIBS "${CLANG_ROOT}/lib/libclang.so")
elseif (EXISTS "${CLANG_ROOT}/lib/libclang.dylib")
  set(CLANG_LIBS "${CLANG_ROOT}/lib/libclang.dylib")
else ()
  find_library(CLANG_LIBS NAMES clang HINTS ${CLANG_ROOT}/lib/ ${CLANG_ROOT}/lib64/llvm/)
endif()

if (EXISTS "${CLANG_INCLUDE}/clang/Basic/Version.inc")
  file(READ "${CLANG_INCLUDE}/clang/Basic/Version.inc" CLANG_VERSION_DATA)
  string(REGEX REPLACE ";" "\\\\;" CLANG_VERSION_DATA ${CLANG_VERSION_DATA})
  string(REGEX REPLACE "\n" ";" CLANG_VERSION_DATA ${CLANG_VERSION_DATA})
  foreach (line ${CLANG_VERSION_DATA})
    string(REGEX REPLACE "^#define CLANG_VERSION ([0-9]+\\.[0-9]+(\\.[0-9]+)?)$" "\\1" CLANG_VERSION_STRING ${line})
    if (DEFINED CLANG_VERSION_STRING)
      string(REGEX REPLACE "^([0-9]+)\\.[0-9]+(\\.[0-9]+)?" "\\1" CLANG_VERSION_MAJOR ${CLANG_VERSION_STRING})
      string(REGEX REPLACE "^[0-9]+\\.([0-9]+)(\\.[0-9]+)?" "\\1" CLANG_VERSION_MINOR ${CLANG_VERSION_STRING})
      if (${CLANG_VERSION_STRING} MATCHES "^[0-9]+\\.[0-9]+\\.[0-9]+")
        string(REGEX REPLACE "^[0-9]+\\.[0-9]+(\\.([0-9]+))?" "\\2" CLANG_VERSION_PATCH ${CLANG_VERSION_STRING})
      else ()
        set(CLANG_VERSION_PATCH "")
      endif ()
      if (NOT ${CLANG_VERSION_MAJOR} STREQUAL "" AND NOT ${CLANG_VERSION_MINOR} STREQUAL "")
        if (NOT ${CLANG_VERSION_PATCH} STREQUAL "")
          set(CLANG_VERSION "${CLANG_VERSION_MAJOR}.${CLANG_VERSION_MINOR}.${CLANG_VERSION_PATCH}")
        else ()
          set(CLANG_VERSION "${CLANG_VERSION_MAJOR}.${CLANG_VERSION_MINOR}")
        endif ()
        break()
      endif ()
    endif ()
  endforeach ()
endif ()
if ("${CLANG_VERSION}" STREQUAL "")
  message(FATAL_ERROR "Unable to parse ClangVersion from ${CLANG_INCLUDE}/clang/Basic/Version.inc")
endif ()

message("Using Clang version ${CLANG_VERSION} from ${CLANG_INCLUDE}/clang-c/")

if (EXISTS "${CLANG_INCLUDE}/clang/${CLANG_VERSION}/include/limits.h")
  set(CLANG_SYSTEM_INCLUDE "${CLANG_INCLUDE}/clang/${CLANG_VERSION}/include/")
else ()
  set(CLANG_SYSTEM_INCLUDE ${CLANG_LIBS})
  string(REGEX REPLACE "\\/libclang\\.[dylibso]+$" "" CLANG_SYSTEM_INCLUDE ${CLANG_SYSTEM_INCLUDE})
  if (EXISTS "${CLANG_SYSTEM_INCLUDE}/clang/${CLANG_VERSION}/include/limits.h")
    set(CLANG_SYSTEM_INCLUDE "${CLANG_SYSTEM_INCLUDE}/clang/${CLANG_VERSION}/include/")
  else ()
    set(CLANG_SYSTEM_INCLUDE "")
    if (EXISTS "${CLANG_ROOT}/lib/clang/${CLANG_VERSION}/include/limits.h")
	set(CLANG_SYSTEM_INCLUDE "${CLANG_ROOT}/lib/clang/${CLANG_VERSION}/include/")
    else ()
	message(FATAL_ERROR "Couldn't find limits.h in either ${CLANG_INCLUDE}/clang/${CLANG_VERSION}/include/ or ${CLANG_SYSTEM_INCLUDE}/clang/${CLANG_VERSION}/include/")
    endif ()
  endif ()
endif ()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(CLANG DEFAULT_MSG CLANG_LIBS CLANG_INCLUDE)

mark_as_advanced(CLANG_INCLUDE CLANG_LIBS)
