function annotate+
  # set leader (head -c (echo (tput cols)' - '(echo $argv | wc -L) | bc) </dev/zero | tr '\0' ' ')
  set indent (echo (tput cols)' - '(echo $argv | wc -L)' - 1' | bc)
  [ $indent -gt 0 ]; and set leader (head -c $indent </dev/zero | tr '\0' ' ')
  log (set_color -o cyan)"@$leader$argv"(set_color normal)
end
