# Find LLVM
#
# It defines the following variables
#  LLVM_FOUND        - True if llvm found.
#  LLVM_INCLUDE_DIRS - where to find llvm include files
#  LLVM_LIBRARY_DIRS - where to find llvm libs
#  LLVM_CFLAGS       - llvm compiler flags
#  LLVM_LDFLAGS      - llvm linker flags
#  LLVM_MODULE_LIBS  - list of llvm libs for working with modules.

set(llvm_config_names llvm-config  
  llvm-config35 llvm-config-3.5 llvm-config-mp-3.5
  llvm-config34 llvm-config-3.4 llvm-config-mp-3.4
  llvm-config33 llvm-config-3.3 llvm-config-mp-3.3
  llvm-config32 llvm-config-3.2 llvm-config-mp-3.2
  llvm-config31 llvm-config-3.1 llvm-config-mp-3.1)
find_program(LLVM_CONFIG_EXECUTABLE NAMES ${llvm_config_names})

if (LLVM_CONFIG_EXECUTABLE)
  message(STATUS "LLVM llvm-config found at: ${LLVM_CONFIG_EXECUTABLE}")
else (LLVM_CONFIG_EXECUTABLE)
  message(FATAL_ERROR "Could NOT find LLVM executable")
endif (LLVM_CONFIG_EXECUTABLE)

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --version
  OUTPUT_VARIABLE LLVM_VERSION
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

string(REGEX REPLACE "^([0-9]+)\\.([0-9]+).*" "\\1" LLVM_VERSION_MAJOR
         "${LLVM_VERSION}")

string(REGEX REPLACE "^([0-9]+)\\.([0-9]+).*" "\\2" LLVM_VERSION_MINOR
         "${LLVM_VERSION}")

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --includedir
  OUTPUT_VARIABLE LLVM_INCLUDE_DIRS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --libdir
  OUTPUT_VARIABLE LLVM_LIBRARY_DIRS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --cppflags
  OUTPUT_VARIABLE LLVM_CFLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)


if (LLVM_CFLAGS MATCHES "\\-DNDEBUG")
    set(LLVM_WITH_NDEBUG TRUE)
else (LLVM_CFLAGS MATCHES "\\-DNDEBUG")
    set(LLVM_WITH_NDEBUG FALSE)
endif (LLVM_CFLAGS MATCHES "\\-DNDEBUG")


execute_process(
  #  COMMAND ${LLVM_CONFIG_EXECUTABLE} --libnames
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --libfiles
  OUTPUT_VARIABLE LLVM_LIBNAMES_STR
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )

string(REGEX REPLACE " +" ";" LLVM_LIBNAMES "${LLVM_LIBNAMES_STR}")

#find_library(LLVM_MODULE_LIBS LLVM-${LLVM_VERSION_MAJOR}.${LLVM_VERSION_MINOR}.a ${LLVM_LIBRARY_DIRS})

message(STATUS "LLVM LibNames: ${LLVM_LIBNAMES_STR}")

find_library(LLVM_MODULE_LIBS ${LLVM_LIBNAMES} ${LLVM_LIBRARY_DIRS})
if (NOT LLVM_MODULE_LIBS)
  execute_process(
    COMMAND ${LLVM_CONFIG_EXECUTABLE} --libs
#    COMMAND ${LLVM_CONFIG_EXECUTABLE} --libfiles
    OUTPUT_VARIABLE LLVM_MODULE_LIBS
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
endif (NOT LLVM_MODULE_LIBS)

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --ldflags
  OUTPUT_VARIABLE LLVM_LDFLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

if (LLVM_CONFIG_EXECUTABLE)
  set(LLVM_FOUND TRUE)
endif (LLVM_CONFIG_EXECUTABLE)
