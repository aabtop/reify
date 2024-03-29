@echo off
SETLOCAL EnableDelayedExpansion

SET START_DIRECTORY=%CD%

SET OUT_DIR=%CD%\out

SET DEPOT_TOOLS_DIR=%~f1
SET V8_SRC_DIR=%~f2
SET BUILD_CONFIG=%3
SET OUTPUT_FILE=%~f4
SET OUTPUT_FILENAME=%~nx4

IF NOT EXIST "%OUT_DIR%" (
  mkdir %OUT_DIR%

  echo Writing args.gn file for %BUILD_CONFIG%...
  (
    echo is_component_build = false
    echo v8_enable_backtrace = true
    echo v8_enable_disassembler = true
    echo v8_enable_object_print = true
    echo v8_enable_verify_heap = true
    echo v8_static_library = true
    echo v8_enable_i18n_support = false
    echo v8_use_external_startup_data = false
    echo is_clang = false
    echo v8_monolithic = true
    echo target_cpu = "x64"
  ) > %OUT_DIR%\args.gn

  IF %BUILD_CONFIG% EQU debug (
    echo is_debug = true
    echo is_official_build = false
  ) >> %OUT_DIR%\args.gn

  IF %BUILD_CONFIG% EQU release (
    echo is_debug = false
    echo is_official_build = true
  ) >> %OUT_DIR%\args.gn

  SET "PATH=%DEPOT_TOOLS_DIR%;%PATH%"

  REM DEPOT_TOOLS_WIN_TOOLCHAIN needs to be set before we `gclient sync` V8, or
  REM else it gives us an error. The variable tells depot_tools that we will
  REM provide our own toolchain.
  SET DEPOT_TOOLS_WIN_TOOLCHAIN=0

  SET DEPOT_TOOLS_UPDATE=0

  REM V8's GN setup enables overriding of the default MSVC path by setting the
  REM environment variable GYP_MSVS_OVERRIDE_PATH, so we do that here if
  REM %BAZEL_VC% is set in order to forward it on to GN.
  IF DEFINED BAZEL_VC SET GYP_MSVS_OVERRIDE_PATH=%BAZEL_VC%\..
  REM Well this certainly will need to be changed when we update VS.
  IF DEFINED BAZEL_VC SET vs2019_install=%BAZEL_VC%\..

  REM This is supposed to be done when `gclient sync` is called and the runhooks
  REM are called, but that seems to not always happen :(.
  cd %V8_SRC_DIR%\build\util
  CALL python lastchange.py -o LASTCHANGE
  cd %V8_SRC_DIR%

  echo Running "gn gen %OUT_DIR%"...
  CALL gn gen %OUT_DIR%
  IF %ERRORLEVEL% NEQ 0 EXIT 1

  echo Running "ninja -C %OUT_DIR%"...
  CALL ninja -C %OUT_DIR% v8_monolith 1> %OUT_DIR%\ninja_output.txt 2>&1
  IF %ERRORLEVEL% NEQ 0 EXIT 1
)

copy "%OUT_DIR%\obj\%OUTPUT_FILENAME%" "%OUTPUT_FILE%"

echo Done.

cd %START_DIRECTORY%
