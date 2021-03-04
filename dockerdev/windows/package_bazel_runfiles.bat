@echo off
SETLOCAL EnableExtensions EnableDelayedExpansion

SET MANIFEST_FILE=%~f1
SET PACKAGE_DIR=%~f2
SET EXECUTABLE_TARGET=%3

for /F "tokens=1*" %%i in (%MANIFEST_FILE%) do (
  SET FILEPATH=%%j
  echo !FILEPATH!| findstr /e %EXECUTABLE_TARGET% >nul 2>nul && (
    SET EXECUTABLE_PATH=!FILEPATH!
  )
)

for %%F in ("!EXECUTABLE_PATH!") do set EXECUTABLE_DIR=%%~dpF

for /F "tokens=1*" %%i in (%MANIFEST_FILE%) do (
  SET FILEPATH_WITH_FORWARDSLASHES=%%j
  for %%F in ("!FILEPATH_WITH_FORWARDSLASHES!") do (
    SET FILEPATH=%%~fF
  )

  set RELATIVE_PATH=!FILEPATH:%EXECUTABLE_DIR%=!
  if !RELATIVE_PATH! NEQ !FILEPATH! (
    set TARGET_FILE=%PACKAGE_DIR%/!RELATIVE_PATH!
    echo F|xcopy /Y "!FILEPATH!" "!TARGET_FILE!"
  )
)
