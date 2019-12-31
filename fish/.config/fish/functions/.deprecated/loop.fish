function loop
  while true
    echo (set_color -o green)(date '+%a %-I:%M:%S%p ')(set_color -o red):loop:(set_color -o yellow) $argv (set_color normal)
    eval $argv
    sleep 30s
  end
end
