# - Find V8
#
#  V8_INCLUDE - Where to find v8.h
#  V8_LIBS    - List of libraries when using V8.
#  V8_FOUND   - True if V8 found.

get_filename_component(module_file_path ${CMAKE_CURRENT_LIST_FILE} PATH)

# Look for the header file.
find_path(V8_INCLUDE NAMES v8.h PATHS $ENV{V8_ROOT}/include /opt/local/include /usr/local/include /usr/include DOC "Path in which the file v8.h is located." )
mark_as_advanced(V8_INCLUDE)

# Look for the library.
# Does this work on UNIX systems? (LINUX)
find_library(V8_LIBS NAMES v8 PATHS /usr/lib $ENV{V8_ROOT}/lib DOC "Path to v8 library." )
mark_as_advanced(V8_LIBS)

# Copy the results to the output variables.
if (V8_INCLUDE AND V8_LIBS)
  message(STATUS "Found v8 in ${V8_INCLUDE} ${V8_LIBS}")
  set(V8_FOUND 1)
  include(CheckCXXSourceCompiles)
  set(CMAKE_REQUIRED_LIBRARIES ${V8_LIBS} pthread)
  set(CMAKE_REQUIRED_INCLUDES ${V8_INCLUDE})

  check_cxx_source_compiles("
    #include <v8.h>

    int main()
    {
        v8::Persistent<v8::Context> ctx;
        ctx.Dispose(0);
        return 0;
     }"
     V8_DISPOSE_HAS_ISOLATE)
 else ()
   set(V8_FOUND 0)
 endif ()

 # Report the results.
 if (NOT V8_FOUND)
   set(V8_DIR_MESSAGE "V8 was not found. Make sure V8_LIBS and V8_INCLUDE are set.")
   if (V8_FIND_REQUIRED)
     message(FATAL_ERROR "${V8_DIR_MESSAGE}")
   elseif (NOT V8_FIND_QUIETLY)
     message(STATUS "${V8_DIR_MESSAGE}")
   endif ()
 endif ()
