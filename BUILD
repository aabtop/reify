cc_library(
    name = "reify",
    srcs = [
        "src/cmake_project/compiler_environment.cc",
        "src/cmake_project/compiled_module.cc",
        "src/cmake_project/compiled_module_impl.h",
        "src/cmake_project/generic_function.cc",
        "src/cmake_project/generic_function_impl.h",
        "src/cmake_project/global_initialization.cc",
        "src/cmake_project/global_initialization.h",
        "src/cmake_project/public_include/reify.h",
        "src/cmake_project/runtime_environment.cc",
        "src/cmake_project/typescript_compiler.cc",
        "src/cmake_project/typescript_compiler.h",
        "src/cmake_project/virtual_filesystem.cc",
    ],
    data = [
        "//src/tsc_wrapper",
        "//third_party/v8",
    ],
)
