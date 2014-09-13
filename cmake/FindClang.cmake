# Find Clang
#
# It defines the following variables
# CLANG_FOUND        - True if Clang found.
# CLANG_INCLUDE_DIRS - where to find Clang include files
# CLANG_LIBS         - list of clang libs
# CLANG_LDFLAGS      - list w/format: -lclangAST -lclangLex...
if (NOT LLVM_INCLUDE_DIRS OR NOT LLVM_LIBRARY_DIRS)
  message(FATAL_ERROR "No LLVM and Clang support requires LLVM")
else (NOT LLVM_INCLUDE_DIRS OR NOT LLVM_LIBRARY_DIRS)

  macro(FIND_AND_ADD_CLANG_LIB _libname_)
    find_library(CLANG_${_libname_}_LIB ${_libname_} ${LLVM_LIBRARY_DIRS} ${CLANG_LIBRARY_DIRS})
    if (CLANG_${_libname_}_LIB)
      set(CLANG_LIBS ${CLANG_LIBS} ${CLANG_${_libname_}_LIB})
      set(CLANG_LDFLAGS ${CLANG_LDFLAGS} "-l${_libname_}")
    endif (CLANG_${_libname_}_LIB)
  endmacro()

  execute_process(
  COMMAND clang --version
  OUTPUT_VARIABLE CLANG_VERSION
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(REGEX REPLACE ".*([0-9]+)\\.([0-9]+)\\.([0-9]+).*" "\\1" CLANG_VERSION_MAJOR
"${CLANG_VERSION}")

string(REGEX REPLACE ".*([0-9]+)\\.([0-9]+)\\.([0-9]+).*" "\\2" CLANG_VERSION_MINOR
"${CLANG_VERSION}")
    message(STATUS "CLANG MAJOR-VERSION: ${CLANG_VERSION_MAJOR}")
    message(STATUS "CLANG MINOR-VERSION: ${CLANG_VERSION_MINOR}")

  # Clang shared library provides just the limited C interface, so it
  # can not be used.  We look for the static libraries.
  # FIND_AND_ADD_CLANG_LIB(clangFrontend)
  # FIND_AND_ADD_CLANG_LIB(clangDriver)
  # FIND_AND_ADD_CLANG_LIB(clangCodeGen)
  # FIND_AND_ADD_CLANG_LIB(clangEdit)
  # FIND_AND_ADD_CLANG_LIB(clangSema)
  # FIND_AND_ADD_CLANG_LIB(clangChecker)
  # FIND_AND_ADD_CLANG_LIB(clangAnalysis)
  # FIND_AND_ADD_CLANG_LIB(clangRewrite)
  # FIND_AND_ADD_CLANG_LIB(clangAST)
  # FIND_AND_ADD_CLANG_LIB(clangParse)
  # FIND_AND_ADD_CLANG_LIB(clangLex)
  # FIND_AND_ADD_CLANG_LIB(clangBasic)
  # FIND_AND_ADD_CLANG_LIB(clang)

  find_and_add_clang_lib(clang)
  find_and_add_clang_lib(clangIndex)
  find_and_add_clang_lib(clangAnalysis)
  find_and_add_clang_lib(clangARCMigrate)
  find_and_add_clang_lib(clangAST)
  find_and_add_clang_lib(clangASTMatchers)
  find_and_add_clang_lib(clangBasic)
  find_and_add_clang_lib(clangCodeGen)
  find_and_add_clang_lib(clangDriver)
  find_and_add_clang_lib(clangEdit)
  find_and_add_clang_lib(clangFormat)
  find_and_add_clang_lib(clangFrontend)
  find_and_add_clang_lib(clangFrontendTool)
  find_and_add_clang_lib(clangLex)
  find_and_add_clang_lib(clangParse)
  find_and_add_clang_lib(clangRewrite)
  # In clang 3.5.0 clangRewriteCore no longer exists
  if (NOT (CLANG_VERSION_MAJOR GREATER 2 AND
      CLANG_VERSION_MINOR GREATER 4))
      find_and_add_clang_lib(clangRewriteCore)
  endif()
  find_and_add_clang_lib(clangRewriteFrontend)
  find_and_add_clang_lib(clangSema)
  find_and_add_clang_lib(clangSerialization)
  find_and_add_clang_lib(clangStaticAnalyzerCheckers)
  find_and_add_clang_lib(clangStaticAnalyzerCore)
  find_and_add_clang_lib(clangStaticAnalyzerFrontend)
  find_and_add_clang_lib(clangTooling)

  find_path(CLANG_INCLUDE_DIRS clang/Basic/Version.h HINTS ${LLVM_INCLUDE_DIRS})

  function(JOIN VALUES GLUE OUTPUT)
    string (REPLACE ";" "${GLUE}" _TMP_STR "${VALUES}")
    set (${OUTPUT} "${_TMP_STR}" PARENT_SCOPE)
  endfunction()

  if (CLANG_LIBS AND CLANG_INCLUDE_DIRS)
    join("${CLANG_LIBS}" " " CLANG_LIBS_STR)
      message(STATUS "Clang libs: " ${CLANG_LIBS_STR})
      set(CLANG_FOUND TRUE)
    endif ()

    if (CLANG_FOUND)
      message(STATUS "Found Clang: ${CLANG_INCLUDE_DIRS}")
    else (CLANG_FOUND)
      if (CLANG_FIND_REQUIRED)
        message(FATAL_ERROR "Could NOT find Clang")
      endif (CLANG_FIND_REQUIRED)
    endif ()

  endif ()
