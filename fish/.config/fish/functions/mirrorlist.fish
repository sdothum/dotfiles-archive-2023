function mirrorlist
  [ -z "$argv" ]; and set country Canada; or set country $argv
  sudo reflector --verbose --country "$country" -l 200 -p http --sort rate --save /etc/pacman.d/mirrorlist
  cat /etc/pacman.d/mirrorlist
end
