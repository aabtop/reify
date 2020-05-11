cmake_minimum_required(VERSION 3.10)

# set the project name and version
project(reify VERSION 1.0)

# specify the C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
set(THREADS_PREFER_PTHREAD_FLAG TRUE)

# Explicitly use "-O0" instead of the default "-Og" since "-Og" seems to be
# emitting code that is difficult to step through.
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -O0 -U_FORTIFY_SOURCE")
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -O0 -U_FORTIFY_SOURCE")


set(REIFY_INTERFACE_GENERATOR_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} CACHE PATH "\
The directory where the Reify interface generator programs can be found. \
If unset, they (the generators) will be generated during the build.")

set(REIFY_INTERFACE_GENERATOR_FILENAME reify-interface-builder)
set(REIFY_INTERFACE_GENERATOR ${REIFY_INTERFACE_GENERATOR_DIRECTORY}/${REIFY_INTERFACE_GENERATOR_FILENAME})

if (${REIFY_INTERFACE_GENERATOR_DIRECTORY} STREQUAL ${CMAKE_CURRENT_BINARY_DIR})
    # Build the TypeScript and C++ V8 Reify interface generators.
    set(REIFY_INTERFACE_DIRECTORY ${CMAKE_SOURCE_DIR}/interface)
    file(GLOB_RECURSE REIFY_INTERFACE_FILES ${REIFY_INTERFACE_DIRECTORY}/*.*)
    set(IDT_DIRECTORY ${CMAKE_SOURCE_DIR}/third_party/idt)
    file(GLOB_RECURSE IDT_FILES ${IDT_DIRECTORY}/*.*)

    # Setup the nix result directory variables.

    set(GENERATED_REIFY_INTERFACE_NIX_RESULT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/reify_interface_builder)

    # nix-build the interface generators, and then copy them into the cmake binary
    # directory.  The copy is performed because cmake seems to have some trouble
    # with dependency tracking surrounding the symbolic links created by Nix.
    add_custom_command(
        OUTPUT ${REIFY_INTERFACE_GENERATOR}
        DEPENDS ${REIFY_INTERFACE_FILES} ${IDT_FILES}
        COMMAND nix-build -o ${GENERATED_REIFY_INTERFACE_NIX_RESULT_DIRECTORY} ${REIFY_INTERFACE_DIRECTORY} &&
                ${CMAKE_COMMAND} -E copy
                ${GENERATED_REIFY_INTERFACE_NIX_RESULT_DIRECTORY}/bin/${REIFY_INTERFACE_GENERATOR_FILENAME}
                ${REIFY_INTERFACE_GENERATOR}

        COMMENT "Building Reify interface generator."
    )
endif(${REIFY_INTERFACE_GENERATOR_DIRECTORY} STREQUAL ${CMAKE_CURRENT_BINARY_DIR})

# Now, using the Reify interface generators built above, build the interfaces.
set(GENERATED_SOURCE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/src_gen)
add_custom_target(ensure_gen_source_directory_exists
    COMMAND ${CMAKE_COMMAND} -E make_directory ${GENERATED_SOURCE_DIRECTORY})
set(GENERATED_REIFY_INTERFACES_DIRECTORY ${GENERATED_SOURCE_DIRECTORY}/reify_interface)
add_custom_target(ensure_gen_reify_interface_directory_exists
    DEPENDS ensure_gen_source_directory_exists
    COMMAND ${CMAKE_COMMAND} -E make_directory ${GENERATED_REIFY_INTERFACES_DIRECTORY})

set(GENERATED_CPP_IMMUT_REF_COUNTED_REIFY_INTERFACE ${GENERATED_REIFY_INTERFACES_DIRECTORY}/reify_cpp_immut_ref_counted_interface.h)
set(GENERATED_CPP_V8_H_REIFY_INTERFACE ${GENERATED_REIFY_INTERFACES_DIRECTORY}/reify_cpp_v8_interface.h)
set(GENERATED_CPP_V8_CC_REIFY_INTERFACE ${GENERATED_REIFY_INTERFACES_DIRECTORY}/reify_cpp_v8_interface.cc)
set(GENERATED_TS_REIFY_INTERFACE ${GENERATED_REIFY_INTERFACES_DIRECTORY}/reify_ts_interface.ts)

add_custom_command(
    OUTPUT ${GENERATED_CPP_IMMUT_REF_COUNTED_REIFY_INTERFACE} ${GENERATED_CPP_V8_H_REIFY_INTERFACE} ${GENERATED_CPP_V8_CC_REIFY_INTERFACE} ${GENERATED_TS_REIFY_INTERFACE}
    DEPENDS ${REIFY_INTERFACE_GENERATOR} ensure_gen_reify_interface_directory_exists
    COMMAND ${REIFY_INTERFACE_GENERATOR} ${GENERATED_REIFY_INTERFACES_DIRECTORY}
    COMMENT "Building Reify pure C++ immutable ref-counted interface."
)

include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_SOURCE_DIR})


# Build our TypeScript Compiler wrapper script.
set(TSC_WRAPPER_DIRECTORY ${CMAKE_SOURCE_DIR}/tsc_wrapper)
file(GLOB_RECURSE TSC_WRAPPER_FILES ${TSC_WRAPPER_DIRECTORY}/*.*)

find_program(NPM npm)
find_program(NPX npx)

# NPM doesn't like it when there is no home directory, so give it one in the
# CMake build dir.
set(TSC_WRAPPER_WORK_DIR ${CMAKE_CURRENT_BINARY_DIR}/tsc_wrapper)
set(TSC_WRAPPER_JS ${TSC_WRAPPER_WORK_DIR}/dist/tsc_wrapper.js)
add_custom_command(
    OUTPUT ${TSC_WRAPPER_JS}
    DEPENDS ${TSC_WRAPPER_FILES}
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${TSC_WRAPPER_DIRECTORY} ${TSC_WRAPPER_WORK_DIR} &&
            cd ${TSC_WRAPPER_WORK_DIR} &&
            ${CMAKE_COMMAND} -E env HOME=${CMAKE_CURRENT_BINARY_DIR}/home ${NPM} install &&
            ${NPX} webpack
    COMMENT "Building TypeScript compiler wrapper script."
)

set(GENERATED_TYPESCRIPT_HEADER ${GENERATED_SOURCE_DIRECTORY}/typescript_compiler_js.h)

add_custom_command(
    OUTPUT ${GENERATED_TYPESCRIPT_HEADER}
    DEPENDS ${TSC_WRAPPER_JS} ${CMAKE_SOURCE_DIR}/embed_binary_as_header.script.cmake
    COMMAND cmake -DBINARYBAKER_INPUT_FILE=${TSC_WRAPPER_JS}
                  -DBINARYBAKER_OUTPUT_FILE=${GENERATED_TYPESCRIPT_HEADER}
                  -DBINARYBAKER_VARIABLE_NAME=typescript_compiler_js
                  -P ${CMAKE_SOURCE_DIR}/embed_binary_as_header.script.cmake
    COMMENT "Converting TypeScript compiler binary into a header file."
)

set(GENERATED_TS_REIFY_INTERFACE_HEADER ${GENERATED_REIFY_INTERFACES_DIRECTORY}/reify_ts_interface.ts.h)
add_custom_command(
    OUTPUT ${GENERATED_TS_REIFY_INTERFACE_HEADER}
    DEPENDS ${GENERATED_TS_REIFY_INTERFACE} ${CMAKE_SOURCE_DIR}/embed_binary_as_header.script.cmake
    COMMAND cmake -DBINARYBAKER_INPUT_FILE=${GENERATED_TS_REIFY_INTERFACE}
                    -DBINARYBAKER_OUTPUT_FILE=${GENERATED_TS_REIFY_INTERFACE_HEADER}
                    -DBINARYBAKER_VARIABLE_NAME=reify_ts_interface_src
                    -P ${CMAKE_SOURCE_DIR}/embed_binary_as_header.script.cmake
    COMMENT "Converting Reify TypeScript interface source into a C++ header file."
)

set(TS_LIB_REIFY ${CMAKE_SOURCE_DIR}/ts_lib/reify.ts)
set(GENERATED_TS_LIB_REIFY_HEADER ${GENERATED_SOURCE_DIRECTORY}/ts_lib_reify.ts.h)
add_custom_command(
    OUTPUT ${GENERATED_TS_LIB_REIFY_HEADER}
    DEPENDS ${TS_LIB_REIFY} ${CMAKE_SOURCE_DIR}/embed_binary_as_header.script.cmake
    COMMAND cmake -DBINARYBAKER_INPUT_FILE=${TS_LIB_REIFY}
                    -DBINARYBAKER_OUTPUT_FILE=${GENERATED_TS_LIB_REIFY_HEADER}
                    -DBINARYBAKER_VARIABLE_NAME=ts_lib_reify_src
                    -P ${CMAKE_SOURCE_DIR}/embed_binary_as_header.script.cmake
    COMMENT "Converting ts_lib/reify.ts into a C++ header file."
)

add_executable(reify
  main.cc
  typescript_compiler.cc
  typescript_compiler.h
  ${GENERATED_CPP_IMMUT_REF_COUNTED_REIFY_INTERFACE}
  ${GENERATED_CPP_V8_H_REIFY_INTERFACE}
  ${GENERATED_CPP_V8_CC_REIFY_INTERFACE}
  ${GENERATED_TYPESCRIPT_HEADER}
  ${GENERATED_TS_REIFY_INTERFACE_HEADER}
  ${GENERATED_TS_LIB_REIFY_HEADER}
)

target_link_libraries(reify v8 pthread)

install(TARGETS reify)
