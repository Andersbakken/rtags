# This file is part of RTags (https://github.com/Andersbakken/rtags).

# RTags is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# RTags is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with RTags.  If not, see <https://www.gnu.org/licenses/>.

set(CMAKE_SKIP_BUILD_RPATH            FALSE)
set(CMAKE_BUILD_WITH_INSTALL_RPATH    FALSE)
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

include(ExternalProject)

if (RTAGS_BUILD_CLANG)
    set(LIBCLANG_VERSION_STRING "5.0.0")
    if (RTAGS_NO_INSTALL)
        set(CLANG_INSTALL_PATH ${CMAKE_CURRENT_BINARY_DIR}/llvmclang-prefix/install)
    else ()
        set(CLANG_INSTALL_PATH ${CMAKE_INSTALL_PREFIX}/rtags-llvmclang)
    endif ()
    ExternalProject_Add(llvmclang
        DOWNLOAD_COMMAND ${PROJECT_SOURCE_DIR}/scripts/getclang.sh "${LIBCLANG_VERSION_STRING}"
        SOURCE_DIR llvmclang-prefix/src/llvm
        CMAKE_ARGS -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=${CLANG_INSTALL_PATH} -DLLVM_BUILD_TOOLS=1
        LOG_DOWNLOAD 1            # Wrap download in script to log output
        LOG_UPDATE 1              # Wrap update in script to log output
        LOG_CONFIGURE 1           # Wrap configure in script to log output
        LOG_BUILD 1               # Wrap build in script to log output
        LOG_TEST 1                # Wrap test in script to log output
        LOG_INSTALL 1             # Wrap install in script to log output
        )
    if (CMAKE_SYSTEM_NAME MATCHES "Darwin")
        set(CLANG_LIBRARY_NAME "libclang.dylib")
        add_definitions("-DCLANG_INCLUDE=${CLANG_INSTALL_PATH}/include/c++/v1") # add libcxx include
    else ()
        set(CLANG_LIBRARY_NAME "libclang.so")
    endif ()
    set(LIBCLANG_LIBRARIES ${CLANG_INSTALL_PATH}/lib/${CLANG_LIBRARY_NAME})
    set(LIBCLANG_LIBDIR ${CLANG_INSTALL_PATH}/lib)
    set(LIBCLANG_CXXFLAGS "-I${CLANG_INSTALL_PATH}/include")
endif ()


configure_file("${CMAKE_CURRENT_SOURCE_DIR}/RTagsVersion.h.in" "${CMAKE_CURRENT_BINARY_DIR}/include/RTagsVersion.h")

# Treat MSYS as Cygwin
if (MSYS)
    set(CYGWIN 1)
    set(CMAKE_SYSTEM_NAME "CYGWIN")
endif ()

# Set RPATH when installing to a custom (non-system) directory
list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
if ("${isSystemDir}" STREQUAL "-1")
    list(APPEND CMAKE_INSTALL_RPATH ${CMAKE_INSTALL_PREFIX}/lib)
endif ()

include(${CMAKE_SOURCE_DIR}/cmake/EnsureLibraries.cmake)

if (NOT DEFINED LIBCLANG_LIBRARIES)
    set(LIBCLANG_LIBRARIES $ENV{LIBCLANG_LIBRARIES})
endif ()

if (NOT DEFINED LIBCLANG_CXXFLAGS)
    set(LIBCLANG_CXXFLAGS $ENV{LIBCLANG_CXXFLAGS})
endif ()

if (NOT DEFINED LIBCLANG_LIBDIR)
    set(LIBCLANG_LIBDIR $ENV{LIBCLANG_LIBDIR})
endif()

if (NOT DEFINED LIBCLANG_LIBRARIES OR NOT DEFINED LIBCLANG_CXXFLAGS OR NOT DEFINED LIBCLANG_LIBDIR)
    find_package(LibClang REQUIRED)
endif ()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${LIBCLANG_CXXFLAGS}")

if (NOT RTAGS_BUILD_CLANG)
    set(LIBCLANG_COMPILE_TEST "
#include <clang-c/Index.h>

int main()
{
    CXIndex index = clang_createIndex(0, true);
    CXTranslationUnit unit = clang_parseTranslationUnit(index, 0, 0, 0, 0, 0, 0);
    clang_disposeTranslationUnit(unit);
    clang_disposeIndex(index);
    return 0;
}
")
    include(CheckCXXSourceCompiles)
    set(CMAKE_REQUIRED_FLAGS ${CMAKE_REQUIRED_FLAGS} ${CMAKE_CXX_FLAGS} ${LIBCLANG_CXXFLAGS})
    set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${LIBCLANG_LIBRARIES})
    check_cxx_source_compiles("${LIBCLANG_COMPILE_TEST}" LIBCLANG_COMPILES)
    if (NOT LIBCLANG_COMPILES)
        message(FATAL_ERROR "Failed to compile small clang test app.\nIt's likely that the include file <clang-c/Index.h> could not be found!\nMaybe you need to install the clang development package (delete the CMakeCache.txt file before trying to run cmake again after installation)?\nSee CMakeFiles/CMakeError.log for more info.")
    endif ()
    unset(CMAKE_REQUIRED_FLAGS)
    unset(CMAKE_REQUIRED_LIBRARIES)
    unset(LIBCLANG_COMPILES)
endif ()

set(CXX_11_EXTENDED_REGEX_TEST "
#include <regex>

int main()
{
  try {
    std::regex rx(\"^(.*):([0-9]+):([0-9]+):?-:?([0-9]+):([0-9]+):?(@[A-Za-z,]+)?\", std::regex_constants::extended);
  } catch (std::regex_error& e) {
    return -1;
  }

  return 0;
}
")
include(CheckCXXSourceRuns)
set(CMAKE_REQUIRED_FLAGS ${CMAKE_REQUIRED_FLAGS} "-std=c++11" ${CMAKE_CXX_FLAGS} ${LIBCLANG_CXXFLAGS})
check_cxx_source_runs("${CXX_11_EXTENDED_REGEX_TEST}" CXX_11_EXTENDED_REGEX_SUPPORT)
if (NOT CXX_11_EXTENDED_REGEX_SUPPORT)
    message(FATAL_ERROR "The compiler uses a libstdc++ without c++11 regex support.")
endif ()
unset(CMAKE_REQUIRED_FLAGS)
unset(CMAKE_REQUIRED_LIBRARIES)
unset(CXX_11_EXTENDED_REGEX_SUPPORT)

if (EXISTS ${CLANG_COMPILATION_INCLUDE})
    add_definitions(-DHAVE_CXCOMPILATIONDATABASE)
endif ()

set(RCT_RTTI_ENABLED 1)
set(RCT_NO_INSTALL 1)
set(RCT_STATIC 1)
set(RCT_NO_LIBRARY 0)
# Everyting which as been set either in rct/rct.cmake or rct/compiler.cmake
# doesn't need to be set in this file again.
include(rct/rct.cmake)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wstrict-aliasing=2 -Wcast-qual -fPIC")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wstrict-aliasing=2 -Wcast-qual -fPIC")
if (NOT CYGWIN)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fstack-protector-all -Wstack-protector")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fstack-protector-all -Wstack-protector")
endif ()

if (CMAKE_BUILD_TYPE MATCHES "Debug")
    add_definitions("-DRTAGS_DEBUG")
    set(RCT_EVENTLOOP_CALLBACK_TIME_THRESHOLD 2000)
endif ()

add_definitions(
    "-DRTAGS_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
    "-DCLANG_LIBDIR=${LIBCLANG_LIBDIR}"
    "-DCLANG_VERSION=${LIBCLANG_VERSION_STRING}"
    "-DOS_${CMAKE_SYSTEM_NAME}"
    ${RCT_DEFINITIONS})

if (CMAKE_SYSTEM_NAME MATCHES "FreeBSD")
    add_definitions(-D__LONG_LONG_SUPPORTED)
endif ()

set(RTAGS_SOURCES
    ClangIndexer.cpp
    ClangThread.cpp
    ClassHierarchyJob.cpp
    CompilerManager.cpp
    CompletionThread.cpp
    DependenciesJob.cpp
    IncludePathJob.cpp
    FileManager.cpp
    FindFileJob.cpp
    FindSymbolsJob.cpp
    FollowLocationJob.cpp
    IncludeFileJob.cpp
    IndexMessage.cpp
    IndexParseData.cpp
    IndexerJob.cpp
    JobScheduler.cpp
    ListSymbolsJob.cpp
    Location.cpp
    Preprocessor.cpp
    Project.cpp
    QueryJob.cpp
    QueryMessage.cpp
    RClient.cpp
    RTags.cpp
    ReferencesJob.cpp
    Sandbox.cpp
    ScanThread.cpp
    Server.cpp
    Source.cpp
    StatusJob.cpp
    Symbol.cpp
    Symbol.cpp
    SymbolInfoJob.cpp
    Token.cpp
    TokensJob.cpp)

add_library(rtags STATIC ${RTAGS_SOURCES})
if (RTAGS_BUILD_CLANG)
    add_dependencies(rtags llvmclang)
endif ()

if (RTAGS_COTIRE)
    include(cotire)
    set_target_properties(rtags PROPERTIES COTIRE_ADD_UNITY_BUILD FALSE)
    cotire(rtags)
endif ()

include_directories(${CMAKE_CURRENT_LIST_DIR} ${RCT_INCLUDE_DIRS})

if (CMAKE_SYSTEM_NAME MATCHES "Darwin")
    set(START_GROUP "")
    set(END_GROUP "")
else ()
    set(START_GROUP "-Wl,--start-group")
    set(END_GROUP "-Wl,--end-group")
endif ()

set(RTAGS_LIBRARIES rtags ${START_GROUP} ${LIBCLANG_LIBRARIES} ${END_GROUP})

if ("${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
    set_source_files_properties(AST.cpp PROPERTIES
        COMPILE_FLAGS "-Wno-unused-parameter -Wno-unused-variable -ftemplate-depth=2000 -Wno-cast-qual -Wno-shadow")
else ()
    set_source_files_properties(AST.cpp PROPERTIES
        COMPILE_FLAGS "-Wno-unused-but-set-parameter -Wno-unused-parameter -Wno-unused-variable -ftemplate-depth=2000 -Wno-cast-qual -Wno-shadow")
endif()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${RTAGS_BINARY_ROOT_DIR}/bin)

# RCT_LIBRARIES and stdc++ library must be at the end
set(RTAGS_LIBRARIES ${RTAGS_LIBRARIES} -lstdc++ ${RCT_LIBRARIES} rct)
add_executable(rc rc.cpp)
target_link_libraries(rc ${RTAGS_LIBRARIES})

add_executable(rdm rdm.cpp)
target_link_libraries(rdm ${RTAGS_LIBRARIES})

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

add_executable(rp rp.cpp)
target_link_libraries(rp ${RTAGS_LIBRARIES})

if (CYGWIN)
    EnsureLibraries(rdm rct)
endif ()

if (EMACS)
    set(EMACS_EXECUTABLE ${EMACS})
else ()
    find_program(EMACS_EXECUTABLE emacs)
endif ()

if (EMACS_EXECUTABLE)
    execute_process(COMMAND ${EMACS_EXECUTABLE} --version
        RESULT_VARIABLE EMACS_ERROR
        OUTPUT_VARIABLE EMACS_VERSION_INFORMATION
        ERROR_QUIET)
    if (NOT EMACS_ERROR)
        string(REGEX MATCH "[0-9]+(\\.[0-9]+)*" EMACS_VERSION ${EMACS_VERSION_INFORMATION})
        if ("${EMACS_VERSION}" LESS "24.3")
            message(WARNING "Installed emacs version is to old, elisp files are not going to be installed, minimum required version is >=24.3!")
            set(RTAGS_NO_ELISP_FILES TRUE)
        else()
            message(STATUS "Found emacs version ${EMACS_VERSION}")
        endif ()
    else ()
        message(STATUS "Could not get emacs version (\"emacs --version\"), elisp files are not going to be installed")
        set(RTAGS_NO_ELISP_FILES TRUE)
    endif ()
else ()
    message(STATUS "Emacs was not found, elisp files are not going to be installed!")
    set(RTAGS_NO_ELISP_FILES TRUE)
endif ()

set(RTAGS_ELISP_FILES
    rtags.el
    ac-rtags.el
    helm-rtags.el
    ivy-rtags.el
    company-rtags.el
    flycheck-rtags.el)

if (NOT RTAGS_NO_ELISP_FILES)
    if ("${EMACS_VERSION}" GREATER "25.0" OR "${EMACS_VERSION}" EQUAL "25.0")
        list(APPEND RTAGS_ELISP_FILES rtags-xref.el)
    endif ()

    if (NOT RTAGS_ELISP_INSTALL_LOCATION)
        set(RTAGS_ELISP_INSTALL_LOCATION ${CMAKE_INSTALL_PREFIX}/share/emacs/site-lisp/rtags/)
    endif ()
    set(RTAGS_ELISP_SOURCES "")
    foreach(el ${RTAGS_ELISP_FILES})
        if (NOT ${CMAKE_CURRENT_BINARY_DIR} STREQUAL ${CMAKE_CURRENT_SOURCE_DIR})
            add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${el}
                COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/${el} ${CMAKE_CURRENT_BINARY_DIR}/${el}
                DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${el}
                COMMENT "Copying ${CMAKE_CURRENT_SOURCE_DIR}/${el} -> ${CMAKE_CURRENT_BINARY_DIR}/${el}")
        endif ()
        list(APPEND RTAGS_ELISP_SOURCES ${CMAKE_CURRENT_BINARY_DIR}/${el})
        if (NOT RTAGS_NO_ELISP_BYTECOMPILE)
            list(APPEND RTAGS_ELISP_SOURCES ${CMAKE_CURRENT_BINARY_DIR}/${el}c)
            add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${el}c
                COMMAND ${EMACS_EXECUTABLE} -batch -l ${CMAKE_CURRENT_SOURCE_DIR}/compile-shim.elisp -l ${CMAKE_CURRENT_SOURCE_DIR}/rtags.el -f batch-byte-compile
                ${CMAKE_CURRENT_BINARY_DIR}/${el}
                DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${el}
                WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                COMMENT "Creating byte-compiled Emacs lisp ${CMAKE_CURRENT_BINARY_DIR}/${el}c")
            add_custom_target(emacs_byte_compile_${el} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${el}c)
        endif ()
    endforeach()
endif ()

install(CODE "message(\"Installing rtags...\")")
include(BashCompletion)
if (BASH_COMPLETION_FOUND)
    file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/completions")
    execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink rtags rc
        COMMAND ${CMAKE_COMMAND} -E create_symlink rtags rdm
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/completions")
    install(FILES rtags-bash-completion.bash DESTINATION ${BASH_COMPLETION_COMPLETIONSDIR}
        PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ
        RENAME rtags)
    install(FILES "${PROJECT_BINARY_DIR}/completions/rc" "${PROJECT_BINARY_DIR}/completions/rdm"
        DESTINATION ${BASH_COMPLETION_COMPLETIONSDIR})
endif ()
install(TARGETS rdm rc rp RUNTIME DESTINATION bin COMPONENT rtags)
install(FILES ../bin/gcc-rtags-wrapper.sh DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
install(FILES ../man/man7/rc.7 ../man/man7/rdm.7 DESTINATION share/man/man7/)

if (UNIX AND NOT ${SKIP_CTEST})
  install(CODE "execute_process(COMMAND ctest)")
endif ()

if (NOT RTAGS_NO_ELISP_FILES)
    install(FILES ${RTAGS_ELISP_SOURCES} DESTINATION ${RTAGS_ELISP_INSTALL_LOCATION})
endif ()

if (CLANGTEST_ENABLED)
    add_executable(clangtest clangtest.cpp)
    target_link_libraries(clangtest ${LIBCLANG_LIBRARIES})
endif ()

if (VAST_ENABLED)
    add_subdirectory(vast)
endif ()
