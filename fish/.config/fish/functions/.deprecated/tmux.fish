function tmux
  switch "$argv[1]"
    case [1];   [ (p /usr/bin/tmux | wc -l) -lt 2 ]; and command tmux attach
    case q;     killall tmux; killall offlineimap
    case '*';   echo ".. tmux  'start  q'uit"
  end
end
