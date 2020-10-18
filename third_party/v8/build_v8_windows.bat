@echo off

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

  echo Running "gclient sync --no-history --shallow --jobs 16"
  CALL gclient sync --no-history --shallow --jobs 16
  IF %ERRORLEVEL% NEQ 0 EXIT 1
)

IF NOT EXIST "%OUT_DIR%" (
  mkdir %OUT_DIR%
  echo Writing args.gn file...
  (
    echo is_component_build = false
    echo is_debug = false
    echo v8_enable_backtrace = true
    echo v8_enable_disassembler = true
    echo v8_enable_object_print = true
    echo v8_enable_verify_heap = true
    echo v8_static_library = true
  ) > %OUT_DIR%\args.gn

  cd %V8_SRC_DIR%

  SET DEPOT_TOOLS_UPDATE=0

  echo Running "gn gen %OUT_DIR%"...
  CALL gn gen %OUT_DIR%
  IF %ERRORLEVEL% NEQ 0 EXIT 1

  echo Running "ninja -C %OUT_DIR%"...
  CALL ninja -C %OUT_DIR%
  IF %ERRORLEVEL% NEQ 0 EXIT 1
)

IF NOT EXIST "%PACKAGE_DIR%" (
  mkdir %PACKAGE_DIR%
  echo Packaging results from build...

  mkdir %PACKAGE_DIR%\win-release-x64-msvc
  cd %PACKAGE_DIR%\win-release-x64-msvc

  xcopy /E /I %V8_SRC_DIR%\include include

  mkdir lib
  FOR %%f in (^
      v8_libbase.lib^
      v8_init.lib^
      ) DO (
    xcopy %OUT_DIR%\obj\%%f lib\ 
  )
)

echo Done.  Find the final results in %PACKAGE_DIR% .

cd %START_DIRECTORY%
