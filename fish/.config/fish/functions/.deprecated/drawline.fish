function drawline
  set linechar ([ (count $argv) -eq 0 ]; and echo '-'; or echo $argv[1])
  set_color red
  head -c (tput cols) < /dev/zero | tr '\0' "$linechar";
  set_color normal
  echo
end
