macro ( QT4_GENERATE_MOCS )
  foreach ( file ${ARGN} )
    get_filename_component ( arg_directory ${file} PATH )
    get_filename_component ( arg_name ${file} NAME )
    set ( moc_file ${arg_directory}/.moc/${arg_name}.moc )
    MAKE_DIRECTORY ( ${arg_directory}/.moc )
    QT4_GENERATE_MOC ( ${file} ${moc_file} )
    include_directories ( ${arg_directory}/.moc )
    macro_add_file_dependencies ( ${file} ${moc_file} )
  endforeach ()
endmacro ()

macro ( QT4_DO_THE_RIGHT_THING outfiles )
  # get include dirs
  QT4_GET_MOC_FLAGS(moc_flags)
  QT4_EXTRACT_OPTIONS(moc_files moc_options ${ARGN})
  MAKE_DIRECTORY ( .moc )

  FOREACH (it ${moc_files})
    GET_FILENAME_COMPONENT(it ${it} ABSOLUTE)
    QT4_MAKE_OUTPUT_FILE(${it} .moc/moc_ cpp outfile)
    QT4_CREATE_MOC_COMMAND(${it} ${outfile} "${moc_flags}" "${moc_options}")
    GET_FILENAME_COMPONENT(path ${outfile} PATH)
    MAKE_DIRECTORY(${path}/.moc)
    SET(${outfiles} ${${outfiles}} ${outfile})
  ENDFOREACH(it)
ENDMACRO (QT4_DO_THE_RIGHT_THING)

include_directories (
        ${CMAKE_CURRENT_LIST_DIR} ${CMAKE_CURRENT_LIST_DIR}/messages
        )

set ( shared_SRCS
        ${CMAKE_CURRENT_LIST_DIR}/Connection.cpp
        ${CMAKE_CURRENT_LIST_DIR}/Path.cpp
        ${CMAKE_CURRENT_LIST_DIR}/Messages.cpp
        ${CMAKE_CURRENT_LIST_DIR}/messages/AddMessage.cpp
        ${CMAKE_CURRENT_LIST_DIR}/messages/QueryMessage.cpp
        ${CMAKE_CURRENT_LIST_DIR}/messages/ErrorMessage.cpp
        ${CMAKE_CURRENT_LIST_DIR}/Log.cpp
        ${CMAKE_CURRENT_LIST_DIR}/Client.cpp
        ${CMAKE_CURRENT_LIST_DIR}/RTags.cpp
        )

set ( shared_HDRS
        ${CMAKE_CURRENT_LIST_DIR}/Connection.h
        ${CMAKE_CURRENT_LIST_DIR}/Path.h
        ${CMAKE_CURRENT_LIST_DIR}/Message.h
        ${CMAKE_CURRENT_LIST_DIR}/Messages.h
        ${CMAKE_CURRENT_LIST_DIR}/messages/AddMessage.h
        ${CMAKE_CURRENT_LIST_DIR}/messages/QueryMessage.h
        ${CMAKE_CURRENT_LIST_DIR}/messages/ErrorMessage.h
        ${CMAKE_CURRENT_LIST_DIR}/RTags.h
        ${CMAKE_CURRENT_LIST_DIR}/Log.h
        ${CMAKE_CURRENT_LIST_DIR}/Client.h
        )

set ( shared_MOCS
        ${CMAKE_CURRENT_LIST_DIR}/Connection.h
        ${CMAKE_CURRENT_LIST_DIR}/Client.h
        ${CMAKE_CURRENT_LIST_DIR}/messages/AddMessage.h
        ${CMAKE_CURRENT_LIST_DIR}/messages/QueryMessage.h
        ${CMAKE_CURRENT_LIST_DIR}/messages/ErrorMessage.h
        ${CMAKE_CURRENT_LIST_DIR}/Message.h
        )

set ( shared_CPPMOCS
        ${CMAKE_CURRENT_LIST_DIR}/Connection.cpp
        )
