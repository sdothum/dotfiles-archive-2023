function pkg_install
  heading+ $argv
  pre_install $argv
  cd ~/sync/dist/archlinux/makepkg/$argv
  sudo makepkg -f --asroot
  cd -
  post_install $argv
end
