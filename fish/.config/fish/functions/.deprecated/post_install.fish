function post_install
  if [ -f ~/sync/dist/archlinux/post_install/$argv ]
    uname -r | grep -q 'ARCH\|ck'
      and echo (set_color -o green)"@ post_install/$argv"(set_color normal)
    eval ~/sync/dist/archlinux/post_install/$argv
  end
end
