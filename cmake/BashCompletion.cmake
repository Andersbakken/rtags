find_package(PkgConfig QUIET)
if(PKG_CONFIG_FOUND)
    execute_process(COMMAND ${PKG_CONFIG_EXECUTABLE} --variable=completionsdir bash-completion
        RESULT_VARIABLE UNAVAILABLE
        OUTPUT_VARIABLE COMPLETIONS_DIR
        ERROR_QUIET
        OUTPUT_STRIP_TRAILING_WHITESPACE)
    if(NOT UNAVAILABLE)
        if(${CMAKE_INSTALL_PREFIX} MATCHES "^/usr")
            string(REGEX REPLACE "^/usr" "" COMPLETIONS_DIR ${COMPLETIONS_DIR})
        endif()
        set(BASH_COMPLETION_COMPLETIONSDIR "${CMAKE_INSTALL_PREFIX}${COMPLETIONS_DIR}"
            CACHE PATH "Bash completion installation directory")
        mark_as_advanced(BASH_COMPLETION_COMPLETIONSDIR)
        set(BASH_COMPLETION_FOUND TRUE)
    endif()
endif()
