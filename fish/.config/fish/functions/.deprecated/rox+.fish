function rox+
  [ -z "(pidof bspwm)" ]; and exit
  bspc query --tree --desktop $argv[1] | grep -q 'ROX-Filer'; and exit
  bspc rule --add ROX-Filer desktop=$argv[1]
  set dirs $argv[(seq 2 (count $argv))]
  for i in $dirs; rox $i 2>/dev/null &; end
  sleep 1s
  bspc rule --remove ROX-Filer
end
