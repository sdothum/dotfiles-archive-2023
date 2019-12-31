function own-sync
  sudo find ~/sync/dist/archlinux/makepkg -user root -exec sudo chown -v shum:users \{\} \;
  sudo find ~/.vim/backups -user root -exec sudo chown -v shum:users \{\} \;
end
