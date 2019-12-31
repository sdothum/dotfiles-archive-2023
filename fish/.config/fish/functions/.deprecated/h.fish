function h
  switch "$argv[1]"
    case [1];   grep 'cmd:' ~/.config/fish/fish_history | sed 's/- cmd: //'
    case m;     h | m
    case '*';   grep 'cmd:' ~/.config/fish/fish_history | sed 's/- cmd: //' | a "$argv"
  end
end
