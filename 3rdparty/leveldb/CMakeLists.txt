cmake_minimum_required( VERSION 2.6 )
set ( CMAKE_BUILD_TYPE Release )

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
