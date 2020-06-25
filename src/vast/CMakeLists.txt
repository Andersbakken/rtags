cmake_minimum_required(VERSION 2.8)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
find_package(Qt5 COMPONENTS Core Widgets REQUIRED)
include_directories(${CMAKE_CURRENT_LIST_DIR} ${Qt5Widgets_INCLUDE_DIRS} ${Qt5Core_INCLUDE_DIRS})
add_executable(vast
               Model.cpp
               Model.cpp
               Node.cpp
               TranslationUnit.cpp
               Window.cpp
               main.cpp)
set_target_properties(vast PROPERTIES AUTOMOC TRUE)
target_link_libraries(vast ${LIBCLANG_LIBRARIES} ${Qt5Widgets_LIBRARIES} ${Qt5Core_LIBRARIES})
