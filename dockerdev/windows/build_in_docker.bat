SET BUILD_CONFIG=opt
SET SCRIPT_DIR=%0\..
SET SRC_DIR=%SCRIPT_DIR%\..\..

echo "In container, building..."

cd %SRC_DIR%

cd projects\hypo
bazel --output_user_root=C:/_bzl build //:hypo //:ide //:visualizer -c %BUILD_CONFIG% --symlink_prefix=/ --verbose_failures
if %errorlevel% neq 0 exit /b %errorlevel%

for /f "usebackq tokens=*" %%a in (`bazel --output_user_root=C:/_bzl info bazel-bin -c %BUILD_CONFIG%`) do (
  powershell -command "& {&'Copy-Item' %%a\hypo.exe -Destination C:/build/out/hypo.exe}"
  CALL %SCRIPT_DIR%\package_bazel_runfiles.bat %%a\ide.exe.runfiles\MANIFEST C:\build\out ide.exe
  CALL %SCRIPT_DIR%\package_bazel_runfiles.bat %%a\visualizer.exe.runfiles\MANIFEST C:\build\out visualizer.exe
)
