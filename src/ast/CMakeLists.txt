cmake_minimum_required(VERSION 2.8)

find_package(V8 REQUIRED)
include_directories(SYSTEM ${V8_INCLUDE_DIR})
add_definitions(${V8_DEFINITIONS})

include_directories(${CMAKE_CURRENT_LIST_DIR})
include_directories(${CMAKE_CURRENT_LIST_DIR}/..)
add_library(ast
            AST.cpp
            Cursor.cpp
            CursorType.cpp
            SourceRange.cpp
            TypeConverter.cpp
            V8SourceLocation.cpp
            V8SourceRange.cpp
            V8CursorType.cpp
            V8Cursor.cpp
            V8Utils.cpp)
target_link_libraries(ast ${V8_LIBS})
