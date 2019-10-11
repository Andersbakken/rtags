cmake_minimum_required(VERSION 2.8)

find_program(NODE NAMES node nodejs PATHS ENV NODE NODE)
find_package(V8 REQUIRED)
include_directories(SYSTEM ${V8_INCLUDE_DIR})
add_definitions(${V8_DEFINITIONS})

include_directories(${CMAKE_CURRENT_LIST_DIR})
include_directories(${CMAKE_CURRENT_LIST_DIR}/..)

add_custom_command(
    OUTPUT V8Cursor.h
           V8Cursor.cpp
           V8CursorType.h
           V8CursorType.cpp
           V8SourceLocation.h
           V8SourceLocation.cpp
           V8SourceRange.h
           V8SourceRange.cpp
    COMMAND node ${CMAKE_CURRENT_LIST_DIR}/../../scripts/generate.js ${CMAKE_CURRENT_LIST_DIR}/ast.json
    DEPENDS ${CMAKE_CURRENT_LIST_DIR}/../../scripts/generate.js
            ${CMAKE_CURRENT_LIST_DIR}/ast.json
            ${CMAKE_CURRENT_LIST_DIR}/../../scripts/V8ClassName.h.template
            ${CMAKE_CURRENT_LIST_DIR}/../../scripts/V8ClassName.cpp.template)
add_library(ast
            AST.cpp
            Cursor.cpp
            CursorType.cpp
            SourceRange.cpp
            TypeConverter.cpp
            ${CMAKE_CURRENT_BINARY_DIR}/V8Cursor.cpp
            ${CMAKE_CURRENT_BINARY_DIR}/V8CursorType.cpp
            ${CMAKE_CURRENT_BINARY_DIR}/V8SourceLocation.cpp
            ${CMAKE_CURRENT_BINARY_DIR}/V8SourceRange.cpp)
target_link_libraries(ast ${V8_LIBS})
