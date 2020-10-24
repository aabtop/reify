load(":v8_rules.bzl", "v8_library")

config_setting(
  name = "dbg_mode",
  values = {
      "compilation_mode": "dbg",
  },
)

v8_library(
  name="v8",
  visibility = ["//visibility:public"],
)
