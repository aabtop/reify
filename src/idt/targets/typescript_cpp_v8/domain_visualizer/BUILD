cc_library(
  name = "interface",
  hdrs = ["include/reify/typescript_cpp_v8/domain_visualizer.h"],
  includes = [
    "include",
  ],
  deps = [
    "//src/idt/targets/typescript_cpp_v8:core",
    "@vulkan_sdk//:vulkan",
  ],
  visibility = ["//visibility:public"],
)

cc_library(
  name = "gui",
  srcs = [
    "domain_visualizer_gui.cc",
  ],
  hdrs = ["include/reify/typescript_cpp_v8/domain_visualizer_gui.h"],
  includes = [
    "include",
  ],
  deps = [
    ":interface",
    "@imgui//:frontend",
    "@imgui//:vulkan_backend",
  ],
  visibility = ["//visibility:public"],
)