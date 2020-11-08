cd /src

cd projects/hypo
bazel build //:hypo -c opt --symlink_prefix=/

cp bazel-bin/hypo /out
