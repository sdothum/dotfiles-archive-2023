function pacaur
  if [ -z $argv ]
    command pacaur -Syu --repo
    command pacaur -Su --aur --noconfirm
    if-no "apply post arch install updates and cleanup"; or ~/sync/dist/archlinux/archlinux-4post
  else
    command pacaur $argv
  end
end
