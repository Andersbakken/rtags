cmake_minimum_required( VERSION 2.6 )
if ( NOT CMAKE_BUILD_TYPE )
	set ( CMAKE_BUILD_TYPE Release )
endif ( NOT CMAKE_BUILD_TYPE )

add_custom_target(
   libleveldb
   ALL
   COMMAND \$\(MAKE\)
   WORKING_DIRECTORY src
   )

add_custom_target (
   copy
   ALL
   COMMAND cp src/libleveldb.a ..
   DEPENDS libleveldb
   )
