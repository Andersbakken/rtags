cmake_minimum_required( VERSION 2.6 )
set ( CMAKE_BUILD_TYPE Release )
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
if ( HAVE_GLIBC )
	target_link_libraries ( makelib -ldl )
else ( HAVE_GLIBC )
	target_link_libraries ( makelib )
endif ( HAVE_GLIBC )
