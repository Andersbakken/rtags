cmake_minimum_required(VERSION 2.8.6)
project(rtags)
set(RTAGS_VERSION_MAJOR 2)
set(RTAGS_VERSION_MINOR 4)
set(RTAGS_VERSION_DATABASE 100)
set(RTAGS_VERSION_SOURCES_FILE 6)
set(RTAGS_VERSION ${RTAGS_VERSION_MAJOR}.${RTAGS_VERSION_MINOR}.${RTAGS_VERSION_DATABASE})

set(CMAKE_LEGACY_CYGWIN_WIN32 0)

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
    include(CPack)
endif ()

feature_summary(INCLUDE_QUIET_PACKAGES WHAT ALL)
