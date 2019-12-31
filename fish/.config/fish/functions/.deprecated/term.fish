function term
  # ln -s only affects new terms, use command cp instead
  switch "$argv"
    case b black;         command cp -f $HOME/.config/Terminal/terminalrc.black $HOME/.config/Terminal/terminalrc
    case c chalkboard;    command cp -f $HOME/.config/Terminal/terminalrc.chalkboard $HOME/.config/Terminal/terminalrc
    case g grey;          command cp -f $HOME/.config/Terminal/terminalrc.grey $HOME/.config/Terminal/terminalrc
    case p paper;         command cp -f $HOME/.config/Terminal/terminalrc.paper $HOME/.config/Terminal/terminalrc
    case o solarized;     command cp -f $HOME/.config/Terminal/terminalrc.solarized $HOME/.config/Terminal/terminalrc
    case t transparent;   command cp -f $HOME/.config/Terminal/terminalrc.transparent $HOME/.config/Terminal/terminalrc
    case r restore    ;   command cp -f $HOME/.config/Terminal/terminalrc.save $HOME/.config/Terminal/terminalrc
    case s save;          command cp -f $HOME/.config/Terminal/terminalrc $HOME/.config/Terminal/terminalrc.save
    case '*';             echo ".. terminal  b'lack  c'halkboard  g'rey  p'aper  r'estore  s'ave  so'larized  t'ransparent"
  end
end
