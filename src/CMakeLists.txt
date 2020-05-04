cmake_minimum_required(VERSION 3.10)

# set the project name and version
project(reify VERSION 1.0)

# specify the C++ standard
#set(CMAKE_CXX_STANDARD 17)
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

set(REIFY_CPP_V8_INTERFACE_GENERATOR_FILENAME reify-interface-builder-cpp-v8)
set(REIFY_TS_INTERFACE_GENERATOR_FILENAME reify-interface-builder-ts)
set(REIFY_CPP_V8_INTERFACE_GENERATOR ${REIFY_INTERFACE_GENERATOR_DIRECTORY}/${REIFY_CPP_V8_INTERFACE_GENERATOR_FILENAME})
set(REIFY_TS_INTERFACE_GENERATOR ${REIFY_INTERFACE_GENERATOR_DIRECTORY}/${REIFY_TS_INTERFACE_GENERATOR_FILENAME})

if (${REIFY_INTERFACE_GENERATOR_DIRECTORY} STREQUAL ${CMAKE_CURRENT_BINARY_DIR})
    # Build the TypeScript and C++ V8 Reify interface generators.
    set(REIFY_INTERFACE_DIRECTORY ${CMAKE_SOURCE_DIR}/interface)
    file(GLOB_RECURSE REIFY_INTERFACE_FILES "*.*")
    set(IDT_DIRECTORY ${CMAKE_SOURCE_DIR}/third_party/idt)
    file(GLOB_RECURSE IDT_FILES "*.*")

    # Setup the nix result directory variables.

    set(GENERATED_REIFY_INTERFACE_NIX_RESULT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/reify_interface_builders)

    # nix-build the interface generators, and then copy them into the cmake binary
    # directory.  The copy is performed because cmake seems to have some trouble
    # with dependency tracking surrounding the symbolic links created by Nix.
    add_custom_command(
        OUTPUT ${REIFY_CPP_V8_INTERFACE_GENERATOR} ${REIFY_TS_INTERFACE_GENERATOR}
        DEPENDS ${REIFY_INTERFACE_FILES} ${IDT_FILES}
        COMMAND nix-build -o ${GENERATED_REIFY_INTERFACE_NIX_RESULT_DIRECTORY} ${REIFY_INTERFACE_DIRECTORY} &&
                ${CMAKE_COMMAND} -E copy
                ${GENERATED_REIFY_INTERFACE_NIX_RESULT_DIRECTORY}/bin/${REIFY_CPP_V8_INTERFACE_GENERATOR_FILENAME}
                ${REIFY_CPP_V8_INTERFACE_GENERATOR} &&
                ${CMAKE_COMMAND} -E copy
                ${GENERATED_REIFY_INTERFACE_NIX_RESULT_DIRECTORY}/bin/${REIFY_TS_INTERFACE_GENERATOR_FILENAME}
                ${REIFY_TS_INTERFACE_GENERATOR}

        COMMENT "Building Reify interface generators."
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

set(GENERATED_CPP_V8_REIFY_INTERFACE ${GENERATED_REIFY_INTERFACES_DIRECTORY}/reify_cpp_v8_interface.h)
set(GENERATED_TS_REIFY_INTERFACE ${GENERATED_REIFY_INTERFACES_DIRECTORY}/reify_ts_interface.ts)

# Build the C++ V8 interface.
add_custom_command(
    OUTPUT ${GENERATED_CPP_V8_REIFY_INTERFACE}
    DEPENDS ${REIFY_CPP_V8_INTERFACE_GENERATOR} ensure_gen_reify_interface_directory_exists
    COMMAND ${REIFY_CPP_V8_INTERFACE_GENERATOR} > ${GENERATED_CPP_V8_REIFY_INTERFACE}
    COMMENT "Building Reify C++ V8 interface."
)

# Build the TypeScript interface.
add_custom_command(
    OUTPUT ${GENERATED_TS_REIFY_INTERFACE}
    DEPENDS ${REIFY_TS_INTERFACE_GENERATOR} ensure_gen_reify_interface_directory_exists
    COMMAND ${REIFY_TS_INTERFACE_GENERATOR} > ${GENERATED_TS_REIFY_INTERFACE}
    COMMENT "Building Reify TypeScript interface."
)


include_directories(${CMAKE_CURRENT_BINARY_DIR})


# Download and extract the TypeScript compiler package.
set(TYPESCRIPT_WORK_DIR ${CMAKE_CURRENT_BINARY_DIR}/typescript)
set(TYPESCRIPT_TAR_FILE typescript-3.8.3.tgz)
file(DOWNLOAD https://registry.npmjs.org/typescript/-/${TYPESCRIPT_TAR_FILE} ${TYPESCRIPT_WORK_DIR}/${TYPESCRIPT_TAR_FILE})
set(TYPESCRIPT_BINARY ${TYPESCRIPT_WORK_DIR}/package/lib/typescript.js)
set(MINIFIED_TYPESCRIPT_BINARY ${TYPESCRIPT_WORK_DIR}/minified_typescript.js)
add_custom_command(
    OUTPUT ${MINIFIED_TYPESCRIPT_BINARY}
    COMMAND cd ${TYPESCRIPT_WORK_DIR} &&
            ${CMAKE_COMMAND} -E tar xfz ${TYPESCRIPT_WORK_DIR}/${TYPESCRIPT_TAR_FILE} &&
            closure-compiler --js ${TYPESCRIPT_BINARY} --js_output_file ${MINIFIED_TYPESCRIPT_BINARY}
    COMMENT "Unpacking TypeScript compiler and minifying it."
)
set(GENERATED_TYPESCRIPT_HEADER ${GENERATED_SOURCE_DIRECTORY}/typescript_compiler_js.h)

add_custom_command(
    OUTPUT ${GENERATED_TYPESCRIPT_HEADER}
    DEPENDS ${MINIFIED_TYPESCRIPT_BINARY} ${CMAKE_SOURCE_DIR}/embed_binary_as_header.script.cmake
    COMMAND cmake -DBINARYBAKER_INPUT_FILE=${MINIFIED_TYPESCRIPT_BINARY}
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
  ${GENERATED_CPP_V8_REIFY_INTERFACE}
  ${GENERATED_TYPESCRIPT_HEADER}
  ${GENERATED_TS_REIFY_INTERFACE_HEADER}
  ${GENERATED_TS_LIB_REIFY_HEADER}
)

target_link_libraries(reify v8 pthread)

install(TARGETS reify)
