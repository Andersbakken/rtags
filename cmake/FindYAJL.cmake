# This will define
# YAJL_FOUND - system has yajl
# YAJL_INCLUDE - include directories necessary to compile w/ yajl
# YAJL_LIBRARY - libraries necessary to link to to get yajl

# Include directories
find_path(YAJL_INCLUDE NAMES yajl/yajl_tree.h)

# Find the library
find_library(YAJL_LIBRARY NAMES yajl)

if (YAJL_INCLUDE AND YAJL_LIBRARY)
  message(STATUS "Found yajl in ${YAJL_INCLUDE} ${YAJL_LIBRARY}")
  set(YAJL_FOUND 1)
else ()
  message(STATUS "yajl not found")
  set(YAJL_FOUND 0)
endif ()
