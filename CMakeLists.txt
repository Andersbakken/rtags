cmake_minimum_required(VERSION 2.8.6)
project(rtags)
set(RTAGS_VERSION_MAJOR 2)
set(RTAGS_VERSION_MINOR 39)
set(RTAGS_VERSION_DATABASE 133)
set(RTAGS_VERSION_SOURCES_FILE 15)
set(RTAGS_VERSION ${RTAGS_VERSION_MAJOR}.${RTAGS_VERSION_MINOR}.${RTAGS_VERSION_DATABASE})
set(RTAGS_BINARY_ROOT_DIR ${PROJECT_BINARY_DIR})

if (RTAGS_ENABLE_DEV_OPTIONS)
    set(CPACK_PACKAGE_INSTALL_DIRECTORY "RTAGS")
    set(CPACK_PACKAGE_VENDOR "RTags")
    set(CPACK_PACKAGE_CONTACT "https://github.com/Andersbakken/rtags")
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
        "\\\\.git/"
        "\\\\.travis.yml$"
        "\\\\.github/"
        "^${PROJECT_SOURCE_DIR}/.dir-locals.el"
        "^${PROJECT_SOURCE_DIR}/build/"
        )

    include(CPack)
else ()
    set(WITH_TESTS 0 CACHE BOOL "Build testing files; requires cppunit.")

    set(RCT_WITH_TESTS ${WITH_TESTS})

    set(CMAKE_LEGACY_CYGWIN_WIN32 0)
    if (CMAKE_COMPILER_IS_GNUCXX)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wzero-as-null-pointer-constant")
    endif ()

    if (APPLE)
        set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -g")
    else ()
        set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -ggdb")
    endif ()

    if (ASAN)
        string(REPLACE "," ";" ASANS ${ASAN})
        foreach (SANITIZE ${ASANS})
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=${SANITIZE}")
            set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fsanitize=${SANITIZE}")
        endforeach ()
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

    include(CTest)

    if (WITH_TESTS)
        find_program(PYTEST NAMES pytest py.test)
        if (PYTEST)
            add_test(NAME automated_tests COMMAND ${PYTEST} -svvv ${CMAKE_SOURCE_DIR}/tests/automated WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/tests/automated)
            set_property(TEST automated_tests PROPERTY ENVIRONMENT "RTAGS_BINARY_DIR=${CMAKE_BINARY_DIR}/bin")
        else()
            message(WARNING "pytest executable not found! Excluding automated_tests from tests.")
        endif ()

        if (EMACS_EXECUTABLE AND NOT RTAGS_NO_ELISP_FILES)
            add_test(NAME elisptests COMMAND ${EMACS_EXECUTABLE} -batch -L ${CMAKE_BINARY_DIR}/src -l ${CMAKE_SOURCE_DIR}/tests/lisp/rtags-test.el -f ert-run-tests-batch-and-exit)
            set_property(TEST elisptests PROPERTY ENVIRONMENT "RTAGS_BINARY_DIR=${CMAKE_BINARY_DIR}/bin")
        else()
            message(WARNING "No suitable Emacs version found! Skipping elisp tests.")
        endif ()
    endif ()

    feature_summary(INCLUDE_QUIET_PACKAGES WHAT ALL)
endif ()
