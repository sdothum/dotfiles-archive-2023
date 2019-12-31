function qwerty
  setxkbmap us -variant basic; left_shift_key; echo qwerty; 
  echo qwerty > ~/.xkbmap
end
