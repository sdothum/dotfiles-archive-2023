function packer
  if [ -z $argv ]
    command packer -Syu
    if-no "apply post arch install updates and cleanup"; or ~/sync/dist/archlinux/archlinux-4post
  else
    command packer $argv
  end
end
