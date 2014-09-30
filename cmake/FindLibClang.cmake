if (NOT DEFINED CLANG_ROOT)
  set(CLANG_ROOT $ENV{CLANG_ROOT})
endif ()

set(llvm_config_names
  llvm-config
  llvm-config35
  llvm-config-3.5
  llvm-config-mp-3.5
  llvm-config34
  llvm-config-3.4
  llvm-config-mp-3.4
  llvm-config33
  llvm-config-3.3
  llvm-config-mp-3.3
  llvm-config32
  llvm-config-3.2
  llvm-config-mp-3.2
  llvm-config31
  llvm-config-3.1
  llvm-config-mp-3.1)
find_program(LLVM_CONFIG_EXECUTABLE NAMES ${llvm_config_names})

if (LLVM_CONFIG_EXECUTABLE)
  message(STATUS "LLVM llvm-config found at: ${LLVM_CONFIG_EXECUTABLE}")
else ()
  message(FATAL_ERROR "Could NOT find LLVM executable.")
endif ()

if (NOT EXISTS ${CLANG_INCLUDE})
  find_path(CLANG_INCLUDE_HACK_CMAKECACHE_DOT_TEXT_BULLSHIT clang-c/Index.h HINTS ${CLANG_ROOT}/include NO_DEFAULT_PATH)
  if (NOT EXISTS ${CLANG_INCLUDE_HACK_CMAKECACHE_DOT_TEXT_BULLSHIT})
    execute_process(COMMAND ${LLVM_CONFIG_EXECUTABLE} --includedir OUTPUT_VARIABLE CLANG_INCLUDE OUTPUT_STRIP_TRAILING_WHITESPACE)
    if (NOT EXISTS ${CLANG_INCLUDE})
      find_path(CLANG_INCLUDE clang-c/Index.h)
      if (NOT EXISTS ${CLANG_INCLUDE})
        message(FATAL_ERROR "Could NOT find clang include path. You can maybe fix this by setting CLANG_INCLUDE in your shell or as a cmake variable.")
      endif ()
    endif ()
  else ()
    set(CLANG_INCLUDE ${CLANG_INCLUDE_HACK_CMAKECACHE_DOT_TEXT_BULLSHIT})
  endif ()
endif ()

if (NOT EXISTS ${CLANG_LIBS})
  find_library(CLANG_LIB_HACK_CMAKECACHE_DOT_TEXT_BULLSHIT NAMES clang libclang ${CLANG_ROOT}/lib NO_DEFAULT_PATH)
  if (NOT EXISTS ${CLANG_CLANG_LIB_HACK_CMAKECACHE_DOT_TEXT_BULLSHIT})
    execute_process(COMMAND ${LLVM_CONFIG_EXECUTABLE} --libdir OUTPUT_VARIABLE CLANG_LIBS OUTPUT_STRIP_TRAILING_WHITESPACE)
    if (NOT EXISTS ${CLANG_LIBS})
      find_library(CLANG_LIBS NAMES clang libclang)
      if (NOT EXISTS ${CLANG_LIBS})
        message(FATAL_ERROR "Could NOT find clang libraries. You can maybe fix this by setting CLANG_LIBS in your shell or as a cmake variable.")
      endif ()
    else ()
      set (CLANG_LIBDIR "${CLANG_LIBS}")
      set (CLANG_LIBS "-L${CLANG_LIBS}" "-lclang" "-Wl,-rpath,${CLANG_LIBS}")
    endif ()
  else ()
    set(CLANG_LIBS "${CLANG_LIB_HACK_CMAKECACHE_DOT_TEXT_BULLSHIT}")
  endif ()
endif ()

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

if (EXISTS "${CLANG_INCLUDE}/clang/${CLANG_VERSION}/include/")
  set(CLANG_SYSTEM_INCLUDE "${CLANG_INCLUDE}/clang/${CLANG_VERSION}/include/")
else ()
  set(CLANG_SYSTEM_INCLUDE ${CLANG_LIBDIR})
  string(FIND "${CLANG_SYSTEM_INCLUDE}" ";" SEMI)
  if (SEMI)
    string(SUBSTRING "${CLANG_SYSTEM_INCLUDE}" 0 ${SEMI} CLANG_SYSTEM_INCLUDE)
  endif ()
  string(REGEX REPLACE "\\/libclang\\.[dylibsoa]+$" "" CLANG_SYSTEM_INCLUDE ${CLANG_SYSTEM_INCLUDE})
  if (EXISTS "${CLANG_SYSTEM_INCLUDE}/clang/${CLANG_VERSION}/include/")
    set(CLANG_SYSTEM_INCLUDE "${CLANG_SYSTEM_INCLUDE}/clang/${CLANG_VERSION}/include/")
  else ()
    if (EXISTS "${CLANG_ROOT}/lib/clang/${CLANG_VERSION}/include/")
      set(CLANG_SYSTEM_INCLUDE "${CLANG_ROOT}/lib/clang/${CLANG_VERSION}/include/")
    else ()
      message("Couldn't find limits.h in either ${CLANG_INCLUDE}/clang/${CLANG_VERSION}/include/ or ${CLANG_SYSTEM_INCLUDE}/clang/${CLANG_VERSION}/include/")
    endif ()
  endif ()
endif ()

message("-- Using Clang version ${CLANG_VERSION} from ${CLANG_INCLUDE}/clang-c/")

