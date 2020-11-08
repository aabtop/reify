cd C:\build\src

cd projects\hypo
bazel build //:hypo -c opt --symlink_prefix=/

copy bazel-bin\hypo.exe c:\build\out
