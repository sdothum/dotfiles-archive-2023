function annotate
  # log (drawline '~')
  # annotate+ $argv
  set indent (echo (tput cols)' - '(echo $argv | wc -L)' - 3' | bc)
  [ $indent -gt 0 ]; and set leader (head -c $indent </dev/zero | tr '\0' '.')
  log (set_color -o cyan)"@ $leader $argv"(set_color normal)
end
