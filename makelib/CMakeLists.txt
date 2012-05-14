cmake_minimum_required( VERSION 2.6 )
set ( CMAKE_BUILD_TYPE Release )
add_definitions ( -Wall )
set ( makelib_HDRS
	
	)

set ( makelib_SRCS
	makelib.cpp
	)

add_library ( makelib MODULE ${makelib_SRCS} ${UIS} ${RSCS} ${TRS} ${MOCS} )
target_link_libraries ( makelib -ldl )
