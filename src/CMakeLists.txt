cmake_minimum_required(VERSION 3.10)

# set the project name and version
project(reify VERSION 1.0)

# specify the C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
set(THREADS_PREFER_PTHREAD_FLAG TRUE)

add_executable(reify main.cc)
target_link_libraries(reify v8 pthread)

install(TARGETS reify)
