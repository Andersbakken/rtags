# - Find V8
# Find the native V8 headers and libraries.
#
#  V8_INCLUDE_DIR -  where to find V8.h, etc.
#  V8_LIBRARIES    - List of libraries when using V8.
#  V8_FOUND        - True if V8 found.

get_filename_component(module_file_path ${CMAKE_CURRENT_LIST_FILE} PATH )

# Look for the header file.
find_path(V8_INCLUDE_DIR NAMES v8.h PATHS /usr/include $ENV{V8_ROOT}/include DOC "Path in which the file v8.h is located." )
mark_as_advanced(V8_INCLUDE_DIR)

# Look for the library.
# Does this work on UNIX systems? (LINUX)
find_library(V8_LIBRARY NAMES v8 PATHS /usr/lib $ENV{V8_ROOT}/lib DOC "Path to v8 library." )
mark_as_advanced(V8_LIBRARY)

# Copy the results to the output variables.
if (V8_INCLUDE_DIR AND V8_LIBRARY)
  set(V8_FOUND 1)
  set(V8_LIBRARIES ${V8_LIBRARY})
  set(V8_INCLUDE_DIR ${V8_INCLUDE_DIR})
  include(CheckCXXSourceCompiles)
  set(CMAKE_REQUIRED_LIBRARIES ${V8_LIBRARIES} pthread)
  set(CMAKE_REQUIRED_INCLUDES ${V8_INCLUDE_DIR})

  check_cxx_source_compiles("
    #include <v8.h>

    int main()
    {
        v8::Persistent<v8::Context> ctx;
        ctx.Dispose(0);
        return 0;
     }"
     V8_DISPOSE_REQUIRES_ARG)
 else ()
   set(V8_FOUND 0)
   set(V8_LIBRARIES)
   set(V8_INCLUDE_DIR)
 endif ()

 # Report the results.
 if (NOT V8_FOUND)
   set(V8_DIR_MESSAGE
     "V8 was not found. Make sure V8_LIBRARY and V8_INCLUDE_DIR are set.")
   if (V8_FIND_REQUIRED)
     message(FATAL_ERROR "${V8_DIR_MESSAGE}")
   elseif (NOT V8_FIND_QUIETLY)
     message(STATUS "${V8_DIR_MESSAGE}")
   endif ()
 endif ()
