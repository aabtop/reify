cmake_minimum_required(VERSION 3.10)

# set the project name and version
project(hypo VERSION 1.0)

# specify the C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
set(THREADS_PREFER_PTHREAD_FLAG TRUE)

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -O0 -U_FORTIFY_SOURCE")
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -O0 -U_FORTIFY_SOURCE")

set(REIFY_HYPO_PATH "" CACHE PATH "\
The directory where the Hypo Reify project source is located.")
if (REIFY_HYPO_PATH STREQUAL "")
  message(FATAL_ERROR "REIFY_HYPO_PATH is not defined.")
endif()

add_subdirectory(${REIFY_HYPO_PATH} reify-hypo)

include_directories(${CMAKE_CURRENT_LIST_DIR} ${OUT_REIFY_PUBLIC_INCLUDE_DIRECTORIES})

add_executable(hypo
  main.cc
  cgal/construct_mesh2.cc
  cgal/construct_mesh2.h
  cgal/construct_mesh3.cc
  cgal/construct_mesh3.h
  cgal/embed_2d_in_3d.cc
  cgal/embed_2d_in_3d.h
  cgal/export_to_stl.cc
  cgal/export_to_stl.h
  cgal/types.h
)

find_package(CGAL REQUIRED)

#required to use basic_viewer
add_definitions(-DCGAL_USE_BASIC_VIEWER -DQT_NO_KEYWORDS)

#link it with the required CGAL libraries
target_link_libraries(hypo CGAL::CGAL ${OUT_REIFY_TARGETS})

install(TARGETS hypo)
