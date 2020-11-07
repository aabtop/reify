@echo off
SETLOCAL EnableDelayedExpansion

SET START_DIRECTORY=%CD%

SET DOWNLOAD_DIR=downloaded_files

SET DEPOT_TOOLS_DIR=%~f1
SET DEPOT_TOOLS_DOWNLOAD_FILE="%DOWNLOAD_DIR%\depot_tools.zip"

SET V8_GCLIENT_DIR=%~f2
SET V8_SRC_DIR=%V8_GCLIENT_DIR%\v8

SET BRANCH=%3

REM DEPOT_TOOLS_WIN_TOOLCHAIN needs to be set before we `gclient sync` V8, or
REM else it gives us an error. The variable tells depot_tools that we will
REM provide our own toolchain.
SET DEPOT_TOOLS_WIN_TOOLCHAIN=0

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

IF NOT EXIST "%V8_GCLIENT_DIR%" (
  mkdir "%V8_GCLIENT_DIR%""
  cd "%V8_GCLIENT_DIR%"

  CALL "%DEPOT_TOOLS_DIR%\git" clone --branch "%BRANCH%" https://github.com/v8/v8.git --depth 1

  echo Writing V8 .gclient file...
  (
    echo solutions = [
    echo   { "name"        : 'v8',
    echo     "url"         : 'https://github.com/v8/v8.git',
    echo     "deps_file"   : 'DEPS',
    echo     "managed"     : False,
    echo     "custom_deps" : {},
    echo     "custom_vars": {},
    echo   },
    echo ]
  ) > .gclient

  echo Running "gclient sync --no-history --shallow"
  CALL %DEPOT_TOOLS_DIR%\gclient sync --no-history --shallow
  IF %ERRORLEVEL% NEQ 0 EXIT 1
)
