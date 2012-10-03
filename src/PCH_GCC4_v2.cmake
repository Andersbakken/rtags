
# (ADD_PCH_RULE  _header_filename _src_list)
# Version 7/26/2010 4:55pm
#
# use this macro before "add_executable"
#
# _header_filename
#	header to make a .gch
#
# _src_list 
#   the variable name (do not use ${..}) which contains a
#     a list of sources (a.cpp b.cpp c.cpp ...)
#  This macro will append a header file to it, then this src_list can be used in
#	"add_executable..."
#
#
# Now a .gch file should be generated and gcc should use it. 
#       	(add -Winvalid-pch to the cpp flags to verify)
#
# make clean should delete the pch file
#
# example : ADD_PCH_RULE(headers.h myprog_SRCS)
MACRO (ADD_PCH_RULE _header_filename _src_list _cflag_list)
  SET(_gch_filename "${_header_filename}.gch")
  LIST(APPEND ${_src_list} ${_gch_filename})
  SET (_args ${CMAKE_CXX_FLAGS})
  LIST(APPEND ${_cflag_list} -include ${_header_filename} -Winvalid-pch)
  LIST(APPEND _args -c ${_header_filename} -o ${_gch_filename})
  GET_DIRECTORY_PROPERTY(DIRINC INCLUDE_DIRECTORIES)
  GET_DIRECTORY_PROPERTY(DIRDEF COMPILE_DEFINITIONS)
  IF ("${CMAKE_BUILD_TYPE}" STREQUAL "")
    LIST(APPEND _args ${CMAKE_CXX_FLAGS})
  ELSE ()
    STRING(TOUPPER ${CMAKE_BUILD_TYPE} BUILDTYPE)
    GET_DIRECTORY_PROPERTY(TYPEDEF DIRECTORY ${CMAKE_CURRENT_LIST_DIR} COMPILE_DEFINITIONS_${BUILDTYPE})
    LIST(APPEND DIRDEF ${TYPEDEF})
    LIST(APPEND _args ${CMAKE_CXX_FLAGS_${BUILDTYPE}})
  ENDIF ()
  foreach (_inc ${DIRINC})
    LIST(APPEND _args "-I" ${_inc})
  endforeach(_inc ${DIRINC})
  foreach (_def ${DIRDEF})
    STRING(REPLACE "\"" "\\\"" _def "${_def}" )
    LIST(APPEND _args -D${_def})
  endforeach(_def ${DIRDEF})
  SEPARATE_ARGUMENTS(_args)
  add_custom_command(OUTPUT ${_gch_filename}
    COMMAND rm -f ${_gch_filename}
    COMMAND ${CMAKE_CXX_COMPILER} ${CMAKE_CXX_COMPILER_ARG1} -x c++-header ${_args}
    DEPENDS ${_header_filename}
            ByteArray.h
            CursorInfo.h
            List.h
            Location.h
            Log.h
            Map.h
            Mutex.h
            Path.h
            RTags.h
            ReadLocker.h
            ReadWriteLock.h
            Serializer.h
            Set.h
            SourceInformation.h
            WaitCondition.h
            WriteLocker.h)
  set_source_files_properties(${${_src_list}} PROPERTIES OBJECT_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${_header_filename})
ENDMACRO()
