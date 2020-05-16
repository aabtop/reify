# rsync the files over from the nix directory so that only the files that
# change are marked as being modified.
rsync -crlpgoD --delete $1 /src

# Run CMake.
mkdir -p /cmake_out
cd /cmake_out
cmake \
  -DREIFY_HYPO_PATH=/src \
  -DCMAKE_BUILD_TYPE=Debug \
  -DCMAKE_FIND_USE_SYSTEM_PACKAGE_REGISTRY=OFF \
  -DCMAKE_FIND_USE_PACKAGE_REGISTRY=OFF \
  $2
cmake --build . -j12