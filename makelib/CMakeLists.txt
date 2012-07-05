cmake_minimum_required( VERSION 2.6 )
if ( NOT CMAKE_BUILD_TYPE )
	set ( CMAKE_BUILD_TYPE Release )
endif ( NOT CMAKE_BUILD_TYPE )
add_definitions ( -Wall )
set ( makelib_HDRS
	
	)

set ( makelib_SRCS
	makelib.cpp
	)

include( CheckCSourceCompiles )

check_c_source_compiles (
	"
	#include <stdlib.h>
	#ifndef __GLIBC__
	#error \"no glibc\"
	#endif
	int main(int argc, char** argv)
	{
		return 0;
	}
	"
	HAVE_GLIBC
	)

add_library ( makelib MODULE ${makelib_SRCS} ${UIS} ${RSCS} ${TRS} ${MOCS} )
if(HAVE_GLIBC)
	target_link_libraries(makelib -ldl)
elseif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
	set(CMAKE_SHARED_MODULE_CREATE_CXX_FLAGS "-flat_namespace -dynamiclib")
	target_link_libraries(makelib)
else()
	target_link_libraries(makelib)
endif()
