function headline
  drawline '='
  heading ' ' "$argv"
  drawline '='
  # uname -r | grep -q MANJARO; and log (set_color -o green)(date '+-%-I:%M %S%P-')(set_color normal)
end
