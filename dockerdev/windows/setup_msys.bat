(
  bash -c "pacman-key --init"
  bash -c "pacman-key --populate msys2"
  bash -c "pacman-key --refresh-keys"
)
echo all done!
exit