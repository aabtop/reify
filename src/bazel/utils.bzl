def convert_to_header(name, input_target, out):
  file_target_name = name + "_file"

  native.genrule(
      name = file_target_name,
      srcs = [input_target],
      outs = ["src_gen/" + out],
      cmd = """
          mkdir -p src_gen
          cp $< $$(basename $<)
          xxd -i $$(basename $<) > $@
          rm $$(basename $<)
      """,
  )

  native.cc_library(
      name = name,
      hdrs = [":" + file_target_name],
      includes = ["."],
      visibility = ["//visibility:public"],
  )