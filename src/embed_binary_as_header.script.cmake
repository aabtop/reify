# Thanks to https://sheredom.wordpress.com/2014/12/09/baking-a-binary-in-a-header-with-cmake-part-1/

# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.

# Takes a file and embeds it in a C header with a given variable name

if(NOT DEFINED BINARYBAKER_INPUT_FILE)
  message(FATAL_ERROR
    "Required cmake variable BINARYBAKER_INPUT_FILE not set!"
  )
endif()

if(NOT DEFINED BINARYBAKER_OUTPUT_FILE)
  message(FATAL_ERROR
    "Required cmake variable BINARYBAKER_OUTPUT_FILE not set!"
  )
endif()

if(NOT DEFINED BINARYBAKER_VARIABLE_NAME)
  message(FATAL_ERROR
    "Required cmake variable BINARYBAKER_VARIABLE_NAME not set!"
  )
endif()

if(NOT EXISTS ${BINARYBAKER_INPUT_FILE})
  message(FATAL_ERROR "File '${BINARYBAKER_INPUT_FILE}' does not exist!")
endif()

file(READ "${BINARYBAKER_INPUT_FILE}" contents HEX)

string(TOUPPER "${BINARYBAKER_OUTPUT_FILE}" header_ifndef)
string(REGEX REPLACE "[^A-Z]" "_" header_ifndef "${header_ifndef}")
set(header_ifndef "__${header_ifndef}__")

file(WRITE "${BINARYBAKER_OUTPUT_FILE}"
  "// This file is distributed under the University of Illinois Open Source\n")
file(APPEND "${BINARYBAKER_OUTPUT_FILE}"
  "// License. See LICENSE.TXT for details.\n")
file(APPEND "${BINARYBAKER_OUTPUT_FILE}" "#ifndef ${header_ifndef}\n")
file(APPEND "${BINARYBAKER_OUTPUT_FILE}" "#define ${header_ifndef}\n")

file(APPEND "${BINARYBAKER_OUTPUT_FILE}" "#ifdef __cplusplus\n")
file(APPEND "${BINARYBAKER_OUTPUT_FILE}" "extern \"C\" {\n")
file(APPEND "${BINARYBAKER_OUTPUT_FILE}" "#endif\n")

file(APPEND "${BINARYBAKER_OUTPUT_FILE}" "const uint8_t "
  "${BINARYBAKER_VARIABLE_NAME}[] = {")

string(REGEX MATCHALL ".." output "${contents}")
string(REGEX REPLACE ";" ",\n  0x" output "${output}")

file(APPEND "${BINARYBAKER_OUTPUT_FILE}" "  0x${output}\n")

file(APPEND "${BINARYBAKER_OUTPUT_FILE}" "};\n")

file(APPEND "${BINARYBAKER_OUTPUT_FILE}" "#ifdef __cplusplus\n")
file(APPEND "${BINARYBAKER_OUTPUT_FILE}" "}\n")
file(APPEND "${BINARYBAKER_OUTPUT_FILE}" "#endif\n")

file(APPEND "${BINARYBAKER_OUTPUT_FILE}" "#endif//${header_ifndef}\n\n")