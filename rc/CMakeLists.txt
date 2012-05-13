cmake_minimum_required( VERSION 2.6 )
find_package ( Qt4 REQUIRED )
include ( ${QT_USE_FILE} )
set ( CMAKE_BUILD_TYPE Release )

add_definitions ( -Wall -DBUILDING_RC )

include_directories (
	${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR}
	${QT_QTCORE_INCLUDE_DIR} ${QT_QTNETWORK_INCLUDE_DIR}
	.)

set ( rc_HDRS
	GccArguments.h
	)

set ( rc_SRCS
	MakefileParser.cpp
	GccArguments.cpp
	main.cpp
	)

set ( rc_MOCS
	MakefileParser.h
	)

include ( ../shared/shared.cmake )

QT4_WRAP_CPP ( MOCS ${rc_MOCS} ${shared_MOCS} )
QT4_GENERATE_MOCS ( ${shared_CPPMOCS} )

add_custom_target ( gperf )

add_custom_command (
	TARGET gperf
	PRE_BUILD
	COMMAND gperf -I -C -l -L C++ gccopts.gperf -Z gccopts_gperf > gccopts_gperf.h
        VERBATIM
	)

add_executable ( rc ${rc_SRCS} ${shared_SRCS} ${UIS} ${RSCS} ${TRS} ${MOCS} )
target_link_libraries ( rc  ${QT_QTCORE_LIBRARY} ${QT_QTNETWORK_LIBRARY} )

add_dependencies ( rc gperf )
