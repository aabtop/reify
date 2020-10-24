@echo off
SETLOCAL EnableDelayedExpansion

SET START_DIRECTORY=%CD%

SET THIS_SCRIPT_PATH=%~dp0

SET WORK_DIR=%THIS_SCRIPT_PATH%build_win
SET FETCH_DIR=%WORK_DIR%\fetch
SET OUT_DIR=%WORK_DIR%\out
SET PACKAGE_DIR=%WORK_DIR%\package

SET DOWNLOAD_DIR=%FETCH_DIR%\downloaded_files

SET DEPOT_TOOLS_DIR=%FETCH_DIR%\depot_tools
SET DEPOT_TOOLS_DOWNLOAD_FILE="%DOWNLOAD_DIR%\depot_tools.zip"

SET V8_GCLIENT_DIR=%FETCH_DIR%\v8
SET V8_SRC_DIR=%V8_GCLIENT_DIR%\v8

REM DEPOT_TOOLS_WIN_TOOLCHAIN needs to be set before we `gclient sync` V8, or
REM else it gives us an error. The variable tells depot_tools that we will
REM provide our own toolchain.
SET DEPOT_TOOLS_WIN_TOOLCHAIN=0

REM List of supported builds.  Each build is customized by if statements later
REM in this file.
SET BUILDS=win-debug-x64-msvc win-release-x64-msvc

IF NOT EXIST %WORK_DIR% mkdir %WORK_DIR%
IF NOT EXIST %FETCH_DIR% mkdir %FETCH_DIR%
IF NOT EXIST %DOWNLOAD_DIR% mkdir %DOWNLOAD_DIR%

IF NOT EXIST "%DEPOT_TOOLS_DIR%" (
  echo Downloading depot_tools...
  IF NOT EXIST "%DEPOT_TOOLS_DOWNLOAD_FILE%" (
    powershell -Command "(New-Object Net.WebClient).DownloadFile('https://storage.googleapis.com/chrome-infra/depot_tools.zip', '%DEPOT_TOOLS_DOWNLOAD_FILE%')"
    IF %ERRORLEVEL% NEQ 0 EXIT 1
  )

  echo Extracting depot_tools archive...
  mkdir %DEPOT_TOOLS_DIR%
  powershell Expand-Archive -DestinationPath %DEPOT_TOOLS_DIR% -Path %DEPOT_TOOLS_DOWNLOAD_FILE%
  IF %ERRORLEVEL% NEQ 0 EXIT 1

  echo Running `gclient` to run depot_tools' self updater...
  cd %DEPOT_TOOLS_DIR%
  CALL gclient
  IF %ERRORLEVEL% NEQ 0 EXIT 1

  echo All done initial depot_tools fetch.
)

set PATH=%DEPOT_TOOLS_DIR%;%PATH%

IF NOT EXIST "%V8_GCLIENT_DIR%" (
  mkdir %V8_GCLIENT_DIR%
  cd %V8_GCLIENT_DIR%

  echo Writing V8 .gclient file...
  (
    echo solutions = [
    echo   { "name"        : 'v8',
    echo     "url"         : 'https://github.com/v8/v8.git@8.6.395.10',
    echo     "deps_file"   : 'DEPS',
    echo     "managed"     : True,
    echo     "custom_deps" : {
    echo     },
    echo     "custom_vars": {},
    echo   },
    echo ]
  ) > .gclient

  echo Running "gclient sync --no-history --shallow"
  CALL gclient sync --no-history --shallow
  IF %ERRORLEVEL% NEQ 0 EXIT 1
)

IF NOT EXIST "%OUT_DIR%" (
  mkdir %OUT_DIR%
  
  FOR %%B IN (%BUILDS%) DO (
    SET BUILD_DIR="%OUT_DIR%\%%B"
    IF NOT EXIST "!BUILD_DIR!" (
      mkdir !BUILD_DIR!
      echo Writing args.gn file for %%B...
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
      ) > !BUILD_DIR!\args.gn

      IF %%B EQU win-debug-x64-msvc (
        echo is_debug = true
        echo is_official_build = false
      ) >> !BUILD_DIR!\args.gn

      IF %%B EQU win-release-x64-msvc (
        echo is_debug = false
        echo is_official_build = true
      ) >> !BUILD_DIR!\args.gn

      cd %V8_SRC_DIR%

      SET DEPOT_TOOLS_UPDATE=0

      echo Running "gn gen !BUILD_DIR!"...
      CALL gn gen !BUILD_DIR!
      IF %ERRORLEVEL% NEQ 0 EXIT 1

      echo Running "ninja -C !BUILD_DIR!"...
      CALL ninja -C !BUILD_DIR!
      IF %ERRORLEVEL% NEQ 0 EXIT 1
    )
  )
)

IF NOT EXIST "%PACKAGE_DIR%" (
  mkdir %PACKAGE_DIR%

  FOR %%B IN (%BUILDS%) DO (
    echo Packaging results from build %%B...

    mkdir %PACKAGE_DIR%\%%B
    cd %PACKAGE_DIR%\%%B

    xcopy /E /I %V8_SRC_DIR%\include include

    mkdir lib
    xcopy %OUT_DIR%\%%B\obj\*.lib lib\
    xcopy %OUT_DIR%\%%B\obj\*.pdb pdb\
  )
)

echo Done.  Find the final results in %PACKAGE_DIR% .

cd %START_DIRECTORY%
