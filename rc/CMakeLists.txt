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
	MakefileParser.h
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
include ( ../shared/PCH_GCC4_v2.cmake )

QT4_WRAP_CPP ( MOCS ${rc_MOCS} ${shared_MOCS} )
QT4_GENERATE_MOCS ( ${shared_CPPMOCS} )

ADD_PCH_RULE ( Pch.h rc_SRCS rc_PCHFLAGS )
add_definitions( ${rc_PCHFLAGS} )

add_custom_command (
	OUTPUT gccopts_gperf.h
	DEPENDS gccopts.gperf
	PRE_BUILD
	COMMAND gperf -I -C -l -L C++ gccopts.gperf -Z gccopts_gperf > gccopts_gperf.h
	VERBATIM
	)

add_custom_target (
	gperf
	DEPENDS gccopts_gperf.h
	)

add_executable ( rc ${rc_SRCS} ${shared_SRCS} ${UIS} ${RSCS} ${TRS} ${MOCS} )
target_link_libraries ( rc  ${QT_QTCORE_LIBRARY} ${QT_QTNETWORK_LIBRARY} )

add_dependencies ( rc gperf )
