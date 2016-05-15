function scrub
  function status
    p scrub | grep -q "btrfs scrub start -Bd /net/$argv[1]"; and set prompt on
    while true
      date "+== $argv[1] -- %H:%M:%S =="
      sudo btrfs scrub status /net/$argv[1]
      p scrub | grep -q "btrfs scrub start -Bd /net/$argv[1]"; or break
      sleep 10s
    end
    [ -z $prompt ]; or read -p press-enter
  end

  switch "$argv[1]"
    case cancel;  sudo btrfs scrub cancel /net/$argv[2]
    case status;  status $argv[2]
    case "*";     sudo btrfs scrub start -Bd /net/$argv &
                  terminator  -T "scrub $argv" --profile=xmonad -x fish -c "scrub status $argv" 2>/dev/null &
  end
end
