cmake_minimum_required(VERSION 2.8.6)
project(rtags)
set(RTAGS_VERSION_MAJOR 2)
set(RTAGS_VERSION_MINOR 14)
set(RTAGS_VERSION_DATABASE 118)
set(RTAGS_VERSION_SOURCES_FILE 12)
set(RTAGS_VERSION ${RTAGS_VERSION_MAJOR}.${RTAGS_VERSION_MINOR}.${RTAGS_VERSION_DATABASE})

set(CMAKE_LEGACY_CYGWIN_WIN32 0)

if (APPLE)
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -g")
else ()
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -ggdb")
endif ()

if (ASAN)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fsanitize=address")
endif ()

if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING
        "Choose the type of build, options are: Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif()

if ("${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
    set(CMAKE_COMPILER_IS_CLANGCXX TRUE)
endif()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/")

include(FeatureSummary)
add_subdirectory(src)

add_custom_target(man
    COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/scripts/gen-man-pages.sh" "${PROJECT_BINARY_DIR}/bin"
    DEPENDS man/man7/rdm.7 man/man7/rc.7)
add_dependencies(man rdm rc)

if (EXISTS "rules.ninja")
    add_custom_target(release COMMAND ${CMAKE_COMMAND} -GNinja -DCMAKE_BUILD_TYPE=Release . WORKING_DIRECTORY .)
    add_custom_target(debug COMMAND ${CMAKE_COMMAND} -GNinja -DCMAKE_BUILD_TYPE=Debug . WORKING_DIRECTORY .)
else()
    add_custom_target(release COMMAND ${CMAKE_COMMAND} -DCMAKE_BUILD_TYPE=Release . WORKING_DIRECTORY .)
    add_custom_target(debug COMMAND ${CMAKE_COMMAND} -DCMAKE_BUILD_TYPE=Debug . WORKING_DIRECTORY .)
endif()

# Create uninstall script and target
configure_file("${CMAKE_MODULE_PATH}/Uninstall.cmake.in" "${CMAKE_BINARY_DIR}/Uninstall.cmake" IMMEDIATE @ONLY)
add_custom_target(uninstall ${CMAKE_COMMAND} -P "${CMAKE_BINARY_DIR}/Uninstall.cmake")

if (RTAGS_ENABLE_DEV_OPTIONS)
    set(CPACK_PACKAGE_INSTALL_DIRECTORY "RTAGS")
    set(CPACK_PACKAGE_VENDOR "RTags")
    set(CPACK_PACKAGE_CONTACT "http://rtags.net")
    set(CPACK_PACKAGE_VERSION_MAJOR ${RTAGS_VERSION_MAJOR})
    set(CPACK_PACKAGE_VERSION_MINOR ${RTAGS_VERSION_MINOR})
    set(CPACK_PACKAGE_VERSION_DATABASE ${RTAGS_VERSION_DATABASE})
    set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.txt")
    set(CPACK_PACKAGE_VERSION "${RTAGS_VERSION}")

    if (NOT CPACK_GENERATOR)
        set(CPACK_GENERATOR TGZ)
    endif ()
    set(CPACK_SOURCE_PACKAGE_FILE_NAME "rtags-${RTAGS_VERSION_MAJOR}.${RTAGS_VERSION_MINOR}"
        CACHE INTERNAL "tarball basename")
    set(CPACK_SOURCE_GENERATOR ${CPACK_GENERATOR})

    set(CPACK_SOURCE_IGNORE_FILES
      "~$"
      "\\\\.gitignore$"
      "\\\\.gitmodules$"
      "^${PROJECT_SOURCE_DIR}/.git/"
      "^${PROJECT_SOURCE_DIR}/build/"
      )

    include(CPack)
endif ()

feature_summary(INCLUDE_QUIET_PACKAGES WHAT ALL)
