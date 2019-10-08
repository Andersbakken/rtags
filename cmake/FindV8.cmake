# Locate V8
# This module defines
# V8_LIBRARY
# V8_FOUND, if false, do not try to link to V8
# V8_INCLUDE_DIR, where to find the headers

find_path(V8_INCLUDE_DIR v8.h
    ${V8_DIR}/include
    $ENV{V8_DIR}/include
    $ENV{V8_DIR}
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/include
    /usr/include
    /sw/include # Fink
    /opt/local/include # DarwinPorts
    /opt/csw/include # Blastwave
    /opt/include
    /usr/freeware/include
    /devel
    )

# On non-Unix platforms (Mac and Windows specifically based on the forum),
# V8 builds separate shared (or at least linkable) libraries for v8_base and v8_snapshot
find_library(V8_BASE_LIBRARY
    NAMES v8_base v8_base.ia32 v8_base.x64 libv8_base v8_libbase
    PATHS
    ${V8_DIR}
    ${V8_DIR}/lib
    ${V8_DIR}/build/Release/lib
    $ENV{V8_DIR}
    $ENV{V8_DIR}/lib
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/lib
    /usr/lib
    /sw/lib
    /opt/local/lib
    /opt/csw/lib
    /opt/lib
    /usr/freeware/lib64
    )

find_library(V8_LIBBASE_LIBRARY
    NAMES v8_libbase v8_libbase.ia32 v8_libbase.x64 libv8_libbase
    PATHS
    ${V8_DIR}
    ${V8_DIR}/lib
    ${V8_DIR}/build/Release/lib
    $ENV{V8_DIR}
    $ENV{V8_DIR}/lib
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/lib
    /usr/lib
    /sw/lib
    /opt/local/lib
    /opt/csw/lib
    /opt/lib
    /usr/freeware/lib64
    )

find_library(V8_LIBPLATFORM_LIBRARY
    NAMES v8_libplatform v8_libplatform.ia32 v8_libplatform.x64 libv8_libplatform
    PATHS
    ${V8_DIR}
    ${V8_DIR}/lib
    ${V8_DIR}/build/Release/lib
    $ENV{V8_DIR}
    $ENV{V8_DIR}/lib
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/lib
    /usr/lib
    /sw/lib
    /opt/local/lib
    /opt/csw/lib
    /opt/lib
    /usr/freeware/lib64
    )

find_library(V8_BASE_LIBRARY_DEBUG
    NAMES v8_base v8_base.ia32 v8_base.x64 libv8_base
    PATHS
    ${V8_DIR}
    ${V8_DIR}/lib
    ${V8_DIR}/build/Debug/lib
    $ENV{V8_DIR}
    $ENV{V8_DIR}/lib
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/lib
    /usr/lib
    /sw/lib
    /opt/local/lib
    /opt/csw/lib
    /opt/lib
    /usr/freeware/lib64
    )

find_library(V8_SNAPSHOT_LIBRARY
    NAMES v8_snapshot libv8_snapshot
    PATHS
    ${V8_DIR}
    ${V8_DIR}/lib
    ${V8_DIR}/build/Release/lib
    $ENV{V8_DIR}
    $ENV{V8_DIR}/lib
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/lib
    /usr/lib
    /sw/lib
    /opt/local/lib
    /opt/csw/lib
    /opt/lib
    /usr/freeware/lib64
    )

find_library(V8_SNAPSHOT_LIBRARY_DEBUG
    NAMES v8_snapshot libv8_snapshot
    PATHS
    ${V8_DIR}
    ${V8_DIR}/lib
    ${V8_DIR}/build/Debug/lib
    $ENV{V8_DIR}
    $ENV{V8_DIR}/lib
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/lib
    /usr/lib
    /sw/lib
    /opt/local/lib
    /opt/csw/lib
    /opt/lib
    /usr/freeware/lib64
    )


# icuuc and icui18n build fine on all platforms
find_library(V8_ICUUC_LIBRARY
    NAMES icuuc libicuuc
    PATHS
    ${V8_DIR}
    ${V8_DIR}/lib
    ${V8_DIR}/build/Release/lib
    ${V8_DIR}/out/ia32.release/lib.target/
    ${V8_DIR}/out/x64.release/lib.target/
    $ENV{V8_DIR}
    $ENV{V8_DIR}/lib
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/lib
    /usr/lib
    /sw/lib
    /opt/local/lib
    /opt/csw/lib
    /opt/lib
    /usr/freeware/lib64
    )

find_library(V8_ICUUC_LIBRARY_DEBUG
    NAMES icuuc libicuuc
    PATHS
    ${V8_DIR}
    ${V8_DIR}/lib
    ${V8_DIR}/build/Debug/lib
    ${V8_DIR}/out/ia32.debug/lib.target/
    ${V8_DIR}/out/x64.debug/lib.target/
    $ENV{V8_DIR}
    $ENV{V8_DIR}/lib
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/lib
    /usr/lib
    /sw/lib
    /opt/local/lib
    /opt/csw/lib
    /opt/lib
    /usr/freeware/lib64
    )

find_library(V8_ICUI18N_LIBRARY
    NAMES icui18n libicui18n
    PATHS
    ${V8_DIR}
    ${V8_DIR}/lib
    ${V8_DIR}/build/Release/lib
    ${V8_DIR}/out/ia32.release/lib.target/
    ${V8_DIR}/out/x64.release/lib.target/
    $ENV{V8_DIR}
    $ENV{V8_DIR}/lib
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/lib
    /usr/lib
    /sw/lib
    /opt/local/lib
    /opt/csw/lib
    /opt/lib
    /usr/freeware/lib64
    )

find_library(V8_ICUI18N_LIBRARY_DEBUG
    NAMES icui18n libicui18n
    PATHS
    ${V8_DIR}
    ${V8_DIR}/lib
    ${V8_DIR}/build/Debug/lib
    ${V8_DIR}/out/ia32.debug/lib.target/
    ${V8_DIR}/out/x64.debug/lib.target/
    $ENV{V8_DIR}
    $ENV{V8_DIR}/lib
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/lib
    /usr/lib
    /sw/lib
    /opt/local/lib
    /opt/csw/lib
    /opt/lib
    /usr/freeware/lib64
    )

find_library(V8_LIBRARY_SELF
    NAMES v8 libv8
    PATHS
    ${V8_DIR}
    ${V8_DIR}/lib
    ${V8_DIR}/build/Release/lib
    $ENV{V8_DIR}
    $ENV{V8_DIR}/lib
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/lib
    /usr/lib
    /sw/lib
    /opt/local/lib
    /opt/csw/lib
    /opt/lib
    /usr/freeware/lib64
    )

set(V8_FOUND "NO")
 message(STATUS BASE "BASE " ${V8_BASE_LIBRARY} "\nSNAPSHOT " ${V8_SNAPSHOT_LIBRARY} "\nICUUC " ${V8_ICUUC_LIBRARY} "\nICUI18N " ${V8_ICUI18N_LIBRARY} "\nINCLUDE " ${V8_INCLUDE_DIR})

if(V8_LIBRARY_SELF AND V8_BASE_LIBRARY AND V8_LIBBASE_LIBRARY AND V8_LIBPLATFORM_LIBRARY AND V8_INCLUDE_DIR) # AND V8_SNAPSHOT_LIBRARY
    set(V8_LIBRARY ${V8_LIBRARY_SELF} ${V8_BASE_LIBRARY} ${V8_LIBBASE_LIBRARY} ${V8_LIBPLATFORM_LIBRARY}) # ${V8_SNAPSHOT_LIBRARY})
    set(V8_FOUND "YES")
endif()
