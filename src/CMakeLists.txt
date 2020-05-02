cmake_minimum_required(VERSION 3.10)

# set the project name and version
project(reify VERSION 1.0)

# specify the C++ standard
#set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
set(THREADS_PREFER_PTHREAD_FLAG TRUE)

# Build the TypeScript and C++ V8 Reify interface generators.
set(REIFY_INTERFACE_DIRECTORY ${CMAKE_SOURCE_DIR}/interface)
file(GLOB_RECURSE REIFY_INTERFACE_FILES "*.*")
set(IDT_DIRECTORY ${CMAKE_SOURCE_DIR}/third_party/idt)
file(GLOB_RECURSE IDT_FILES "*.*")

# Setup the nix result directory variables.
set(GENERATED_REIFY_INTERFACE_NIX_RESULT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/reify_interface_builders)

set(REIFY_CPP_V8_INTERFACE_GENERATOR_FILENAME reify-interface-builder-cpp-v8)
set(REIFY_TS_INTERFACE_GENERATOR_FILENAME reify-interface-builder-ts)

set(REIFY_CPP_V8_INTERFACE_GENERATOR ${CMAKE_CURRENT_BINARY_DIR}/${REIFY_CPP_V8_INTERFACE_GENERATOR_FILENAME})
set(REIFY_TS_INTERFACE_GENERATOR ${CMAKE_CURRENT_BINARY_DIR}/${REIFY_TS_INTERFACE_GENERATOR_FILENAME})

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


include_directories(${GENERATED_SOURCE_DIRECTORY})

add_executable(reify
  main.cc
  ${GENERATED_CPP_V8_REIFY_INTERFACE}
)

target_link_libraries(reify v8 pthread)

install(TARGETS reify)
