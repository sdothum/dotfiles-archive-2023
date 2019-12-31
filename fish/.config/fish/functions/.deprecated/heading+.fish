function heading+
  echo
  # heading '~' "$argv"
  heading $argv
  uname -r | grep -q 'ARCH\|ck'
    and echo (set_color -o green)(date '+@ %a %-I:%M %S%P')(set_color normal)
    or echo
end
