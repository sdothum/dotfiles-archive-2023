function capslock
  if [ "$__backspace" = "Caps_Lock" ]
    set -g __backspace
    xmodmap -e "keycode 22 = BackSpace"
  else
    set -g __backspace Caps_Lock
    xmodmap -e "keycode 22 = Caps_Lock"
  end
end
