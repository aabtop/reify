SET BUILD_CONFIG=opt

echo "In container, building..."

cd C:\build\src

cd projects\hypo
bazel --output_user_root=C:/_bzl build //:hypo //src/ide:ide -c %BUILD_CONFIG% --symlink_prefix=/ --verbose_failures
if %errorlevel% neq 0 exit /b %errorlevel%

for /f "usebackq tokens=*" %%a in (`bazel --output_user_root=C:/_bzl info bazel-bin -c %BUILD_CONFIG%`) do (
  powershell -command "& {&'Copy-Item' %%a\hypo.exe -Destination C:/build/out/hypo.exe}"
)
