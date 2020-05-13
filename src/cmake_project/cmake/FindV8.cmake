find_path(V8_INCLUDE_DIR v8.h)
find_library(V8_LIBRARY v8)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(V8
  FOUND_VAR V8_FOUND
  REQUIRED_VARS
    V8_LIBRARY
    V8_INCLUDE_DIR
)
