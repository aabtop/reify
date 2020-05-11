cmake_minimum_required(VERSION 3.10)

set(REIFY_INTERFACE_NAMESPACE "no-namespace")

# Include a generated file that contains CMake settings specific to this Reify
# project.
include(src_gen/generated_reify_options.cmake)

# set the project name and version
project(reify-${REIFY_INTERFACE_NAMESPACE} VERSION 1.0)

# specify the C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
set(THREADS_PREFER_PTHREAD_FLAG TRUE)

add_compile_definitions(REIFY_INTERFACE_NAMESPACE="${REIFY_INTERFACE_NAMESPACE}")

# Explicitly use "-O0" instead of the default "-Og" since "-Og" seems to be
# emitting code that is difficult to step through.
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -O0 -U_FORTIFY_SOURCE")
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -O0 -U_FORTIFY_SOURCE")

include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_SOURCE_DIR})

set(GENERATED_SOURCE_DIRECTORY ${CMAKE_SOURCE_DIR}/src_gen)
set(GENERATED_REIFY_CPP_INTERFACES_DIRECTORY ${GENERATED_SOURCE_DIRECTORY}/interface/cpp)

set(GENERATED_CPP_IMMUT_REF_COUNTED_REIFY_INTERFACE ${GENERATED_REIFY_CPP_INTERFACES_DIRECTORY}/reify_cpp_immut_ref_counted_interface.h)
set(GENERATED_CPP_V8_H_REIFY_INTERFACE ${GENERATED_REIFY_CPP_INTERFACES_DIRECTORY}/reify_cpp_v8_interface.h)
set(GENERATED_CPP_V8_CC_REIFY_INTERFACE ${GENERATED_REIFY_CPP_INTERFACES_DIRECTORY}/reify_cpp_v8_interface.cc)

set(GENERATED_TYPESCRIPT_HEADER ${GENERATED_SOURCE_DIRECTORY}/tsc_wrapper.h)
set(GENERATED_TS_LIB_REIFY_HEADER ${GENERATED_SOURCE_DIRECTORY}/reify_interface_ts.h)
set(GENERATED_TS_REIFY_INTERFACE_HEADER ${GENERATED_SOURCE_DIRECTORY}/reify_generated_interface_ts.h)

set(TARGET_NAME reify-${REIFY_INTERFACE_NAMESPACE})

add_executable(${TARGET_NAME}
  main.cc
  typescript_compiler.cc
  typescript_compiler.h
  ${GENERATED_CPP_IMMUT_REF_COUNTED_REIFY_INTERFACE}
  ${GENERATED_CPP_V8_H_REIFY_INTERFACE}
  ${GENERATED_CPP_V8_CC_REIFY_INTERFACE}
  ${GENERATED_TYPESCRIPT_HEADER}
  ${GENERATED_TS_LIB_REIFY_HEADER}
  ${GENERATED_TS_REIFY_INTERFACE_HEADER}
)

target_link_libraries(${TARGET_NAME} v8 pthread)

install(TARGETS ${TARGET_NAME})
