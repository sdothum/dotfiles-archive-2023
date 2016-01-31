function install+
  if [ ! -f .install ]
    # drawline ':'
    echo
  end
  trace (set_color -o red)"─── installing ───" (set_color -o cyan)"$argv"
  echo $argv >> .install
end
