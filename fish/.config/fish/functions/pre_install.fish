function pre_install
  if [ -f ~/sync/dist/archlinux/pre_install/$argv ]
    uname -r | grep -q 'ARCH\|ck'
      and echo (set_color -o green)"@ pre_install/$argv"(set_color normal)
    eval ~/sync/dist/archlinux/pre_install/$argv
  end
end
