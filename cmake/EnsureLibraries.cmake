#### EnsureLibraries

# Macro for copying needed libraries to target directory
MACRO(EnsureLibraries EXE_NAME LIBRARY_NAMES )
    
    FOREACH ( LIBRARY_NAME IN ITEMS ${LIBRARY_NAMES} )
        GET_TARGET_PROPERTY ( libLocation "${LIBRARY_NAME}" LOCATION )

        IF ( NOT "${libLocation}" STREQUAL "libLocation-NOTFOUND" )

            GET_TARGET_PROPERTY ( exeLocation "${EXE_NAME}" LOCATION )

            IF ( CMAKE_COMPILER_IS_GNUCC )
                ADD_CUSTOM_COMMAND ( TARGET ${EXE_NAME}
                    POST_BUILD
		    COMMAND ${CMAKE_COMMAND} -D LIBRARY="${libLocation}" -D TARGET="${exeLocation}" -P "${CMAKE_SOURCE_DIR}/cmake/EnsureLibrariesRunner.cmake"
                    COMMENT "Copy lib ${LIBRARY_NAME} to ${EXE_NAME} build ..." 
                    DEPENDS ${LIBRARY_NAME}
                )
            ELSE ( CMAKE_COMPILER_IS_GNUCC )

                INCLUDE(GenerateExportHeader)
                
                IF ( NOT CYGWIN )
                    GENERATE_EXPORT_HEADER( ${LIBRARY_NAME}
                        BASE_NAME ${LIBRARY_NAME}
                        EXPORT_MACRO_NAME ${LIBRARY_NAME}_EXPORT
                        EXPORT_FILE_NAME ${LIBRARY_NAME}_Export.h
                        STATIC_DEFINE ${LIBRARY_NAME}_BUILT_AS_STATIC
                    )
                ENDIF ( NOT CYGWIN )
            
                ADD_CUSTOM_COMMAND ( TARGET ${EXE_NAME}
                    POST_BUILD
                    COMMAND ${CMAKE_COMMAND} -E copy "${libLocation}" "$<TARGET_FILE_DIR:${EXE_NAME}>" 
                    COMMENT "Copy lib ${LIBRARY_NAME} to ${EXE_NAME} build ..." 
                    DEPENDS ${LIBRARY_NAME} 
                    DEPENDS osswin
                )
            ENDIF ( CMAKE_COMPILER_IS_GNUCC )

        ENDIF ( NOT "${libLocation}" STREQUAL "libLocation-NOTFOUND" )

    ENDFOREACH ( LIBRARY_NAME IN ITEMS ${LIBRARY_NAMES} )

ENDMACRO(EnsureLibraries)
