set(CMAKE_SKIP_BUILD_RPATH            FALSE)
set(CMAKE_BUILD_WITH_INSTALL_RPATH    FALSE)
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

include(ExternalProject)

if (RTAGS_BUILD_CLANG)
    if (RTAGS_NO_INSTALL)
        set(CLANG_INSTALL_PATH ${CMAKE_CURRENT_BINARY_DIR}/llvmclang-prefix/install)
    else ()
        set(CLANG_INSTALL_PATH ${CMAKE_INSTALL_PREFIX}/rtags-llvmclang)
    endif ()
    ExternalProject_Add(llvmclang
        DOWNLOAD_COMMAND ${PROJECT_SOURCE_DIR}/scripts/getclang.sh 3.8.1
        SOURCE_DIR llvmclang-prefix/src/llvm
        CMAKE_ARGS -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=${CLANG_INSTALL_PATH} -DLLVM_BUILD_TOOLS=0
        LOG_DOWNLOAD 1            # Wrap download in script to log output
        LOG_UPDATE 1              # Wrap update in script to log output
        LOG_CONFIGURE 1           # Wrap configure in script to log output
        LOG_BUILD 1               # Wrap build in script to log output
        LOG_TEST 1                # Wrap test in script to log output
        LOG_INSTALL 1             # Wrap install in script to log output
        )
    set(LIBCLANG_LIBRARIES "${CLANG_INSTALL_PATH}/lib/libclang${CMAKE_SHARED_LIBRARY_SUFFIX}")
    set(LIBCLANG_CXXFLAGS "-I${CLANG_INSTALL_PATH}/include -fPIC  -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS")
    set(LIBCLANG_LIBDIR "${CLANG_INSTALL_PATH}/lib")
    if (CMAKE_SYSTEM_NAME MATCHES "Darwin")
        add_definitions("-DCLANG_INCLUDE=${CLANG_INSTALL_PATH}/include/c++/v1") # add libcxx include
    endif ()
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
    set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_RPATH}:${CMAKE_INSTALL_PREFIX}/lib")
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

if (EXISTS ${CLANG_COMPILATION_INCLUDE})
    add_definitions(-DHAVE_CXCOMPILATIONDATABASE)
endif ()

if (LUA_ENABLED)
    ExternalProject_Add(lua
        DOWNLOAD_COMMAND ""
        SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/lua
        INSTALL_COMMAND ""
        CMAKE_ARGS -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=0 -DCMAKE_GENERATOR=${CMAKE_GENERATOR})
endif ()

set(RCT_RTTI_ENABLED 1)
set(RCT_NO_INSTALL 1)
set(RCT_NO_LIBRARY 1)
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
    "-DOS_${CMAKE_SYSTEM_NAME}"
    ${RCT_DEFINITIONS})

if (CMAKE_SYSTEM_NAME MATCHES "FreeBSD")
    add_definitions(-D__LONG_LONG_SUPPORTED)
endif ()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

set(RTAGS_SOURCES
    ClangIndexer.cpp
    ClangThread.cpp
    ClassHierarchyJob.cpp
    CompilerManager.cpp
    CompletionThread.cpp
    DependenciesJob.cpp
    FileManager.cpp
    FindFileJob.cpp
    FindSymbolsJob.cpp
    FollowLocationJob.cpp
    IncludeFileJob.cpp
    IndexMessage.cpp
    IndexerJob.cpp
    JobScheduler.cpp
    ListSymbolsJob.cpp
    Location.cpp
    Preprocessor.cpp
    ProcThread.cpp
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
    TokensJob.cpp
    ${RCT_SOURCES})

if (LUA_ENABLED)
    list(APPEND RTAGS_SOURCES AST.cpp)
endif ()

add_library(rtags STATIC ${RTAGS_SOURCES})
if (RTAGS_BUILD_CLANG)
    add_dependencies(rtags llvmclang)
endif ()
if (LUA_ENABLED)
    add_dependencies(rtags lua)
endif ()

if (RTAGS_COTIRE)
    include(cotire)
    set_target_properties(rtags PROPERTIES COTIRE_ADD_UNITY_BUILD FALSE)
    cotire(rtags)
endif ()

include_directories(
    ${CMAKE_CURRENT_LIST_DIR}
    ${RCT_INCLUDE_DIRS}
    ${CMAKE_CURRENT_LIST_DIR}/selene/include
    ${CMAKE_CURRENT_BINARY_DIR}/lua-prefix/src/lua-build
    ${CMAKE_CURRENT_LIST_DIR}/lua/src)

if (CMAKE_SYSTEM_NAME MATCHES "Darwin")
    set(START_GROUP "")
    set(END_GROUP "")
else ()
    set(START_GROUP "-Wl,--start-group")
    set(END_GROUP "-Wl,--end-group")
endif ()

set(RTAGS_LIBRARIES rtags ${START_GROUP} ${LIBCLANG_LIBRARIES} ${END_GROUP})
if (LUA_ENABLED)
    list(APPEND RTAGS_LIBRARIES -L${CMAKE_CURRENT_BINARY_DIR}/lua-prefix/src/lua-build/ liblua.a)
    add_definitions(-DRTAGS_HAS_LUA)
endif ()

if ("${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
    set_source_files_properties(AST.cpp PROPERTIES
        COMPILE_FLAGS "-Wno-unused-parameter -Wno-unused-variable -ftemplate-depth=2000 -Wno-cast-qual")
else ()
    set_source_files_properties(AST.cpp PROPERTIES
        COMPILE_FLAGS "-Wno-unused-but-set-parameter -Wno-unused-parameter -Wno-unused-variable -ftemplate-depth=2000 -Wno-cast-qual")
endif()

# RCT_LIBRARIES and stdc++ library must be at the end
set(RTAGS_LIBRARIES ${RTAGS_LIBRARIES} -lstdc++ ${RCT_LIBRARIES})
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
        if ("${EMACS_VERSION}" LESS "23")
            message(STATUS "Installed emacs version is to old, elisp files are not going to be installed, minimum required version is >=23!")
            set(RTAGS_NO_ELISP_FILES TRUE)
        endif ()
    else ()
        message(STATUS "Could not get emacs version (\"emacs --version\"), elisp files are not going to be installed")
        set(RTAGS_NO_ELISP_FILES TRUE)
    endif ()
else ()
    set(RTAGS_NO_ELISP_FILES TRUE)
    message(STATUS "Emacs was not found, elisp files are not going to be installed!")
endif ()

set(RTAGS_ELISP_FILES
    rtags.el
    rtags-ac.el
    company-rtags.el
    flycheck-rtags.el)

if (LUA_ENABLED AND NOT RTAGS_NO_LUA_FILES AND LUA_FOUND AND NOT ${CMAKE_CURRENT_BINARY_DIR} STREQUAL ${CMAKE_CURRENT_SOURCE_DIR})
    add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/rtags.lua
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/rtags.lua ${CMAKE_CURRENT_BINARY_DIR}/rtags.lua
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/rtags.lua
        COMMENT "Copying ${CMAKE_CURRENT_SOURCE_DIR}/rtags.lua -> ${CMAKE_CURRENT_BINARY_DIR}/rtags.lua")
endif ()

if (NOT RTAGS_NO_ELISP_FILES)
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
if (NOT RTAGS_NO_ELISP_FILES)
    install(FILES ${RTAGS_ELISP_SOURCES} DESTINATION ${RTAGS_ELISP_INSTALL_LOCATION})
endif ()
