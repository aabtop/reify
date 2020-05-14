cmake_minimum_required(VERSION 3.10)

set(REIFY_GENERATED_PROJECT_NAMESPACE "no-namespace")

# Include a generated file that contains CMake settings specific to this Reify
# project.
include(src_gen/generated_reify_options.cmake)

# set the project name and version
project(reify-${REIFY_GENERATED_PROJECT_NAMESPACE} VERSION 1.0)

# specify the C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
set(THREADS_PREFER_PTHREAD_FLAG TRUE)

set(CMAKE_UTILS_DIR "${CMAKE_CURRENT_LIST_DIR}/cmake")
list(APPEND CMAKE_MODULE_PATH ${CMAKE_UTILS_DIR})
find_package(V8 REQUIRED)

# Explicitly use "-O0" instead of the default "-Og" since "-Og" seems to be
# emitting code that is difficult to step through.
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -O0 -U_FORTIFY_SOURCE")
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -O0 -U_FORTIFY_SOURCE")

set(GENERATED_SOURCE_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/src_gen)
set(GENERATED_REIFY_CPP_INTERFACES_DIRECTORY ${GENERATED_SOURCE_DIRECTORY}/interface/cpp)

set(GENERATED_REIFY_INTERFACE ${GENERATED_SOURCE_DIRECTORY}/public_include/${REIFY_GENERATED_PROJECT_NAMESPACE}.h)
set(GENERATED_CPP_V8_H_REIFY_INTERFACE ${GENERATED_SOURCE_DIRECTORY}/public_include/reify_cpp_v8_interface.h)
set(GENERATED_CPP_V8_CC_REIFY_INTERFACE ${GENERATED_REIFY_CPP_INTERFACES_DIRECTORY}/reify_cpp_v8_interface.cc)

set(GENERATED_TYPESCRIPT_HEADER ${GENERATED_SOURCE_DIRECTORY}/tsc_wrapper.h)
set(GENERATED_TS_LIB_REIFY_HEADER ${GENERATED_SOURCE_DIRECTORY}/reify_interface_ts.h)
set(GENERATED_TS_REIFY_INTERFACE_HEADER ${GENERATED_SOURCE_DIRECTORY}/reify_generated_interface_ts.h)

set(TARGET_NAME reify-${REIFY_GENERATED_PROJECT_NAMESPACE})

set(REIFY_PUBLIC_INCLUDE_DIRECTORIES ${CMAKE_CURRENT_LIST_DIR}/public_include ${CMAKE_CURRENT_LIST_DIR}/src_gen/public_include ${V8_INCLUDE_DIR})
set(OUT_REIFY_PUBLIC_INCLUDE_DIRECTORIES ${REIFY_PUBLIC_INCLUDE_DIRECTORIES} PARENT_SCOPE)
include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_LIST_DIR} ${REIFY_PUBLIC_INCLUDE_DIRECTORIES})

add_library(${TARGET_NAME}
  compiler_environment.cc
  compiled_module.cc
  compiled_module_impl.h
  generic_function.cc
  generic_function_impl.h
  global_initialization.cc
  global_initialization.h
  public_include/reify.h
  runtime_environment.cc
  typescript_compiler.cc
  typescript_compiler.h
  ${GENERATED_REIFY_INTERFACE}
  ${GENERATED_CPP_V8_H_REIFY_INTERFACE}
  ${GENERATED_CPP_V8_CC_REIFY_INTERFACE}
  ${GENERATED_TYPESCRIPT_HEADER}
  ${GENERATED_TS_LIB_REIFY_HEADER}
  ${GENERATED_TS_REIFY_INTERFACE_HEADER}
)

set(REIFY_DEPENDENT_TARGETS ${V8_LIBRARY} pthread)
target_link_libraries(${TARGET_NAME} PUBLIC ${REIFY_DEPENDENT_TARGETS})

set(OUT_REIFY_TARGETS ${TARGET_NAME} ${REIFY_DEPENDENT_TARGETS} PARENT_SCOPE)
