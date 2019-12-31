function install
  # see apt.fish for apt.error
  [ -n "$aptmode" ]; and set action $aptmode; or set action i
  # echo action $action
  if [ "$action" = s ]
    apt s $argv | grep '^i'
    return
  else
    # for session terminal window visuals
    # drawline ":"
    # echo
  if [ ! -f .install ]
      echo
    end
    trace (set_color -o red)"─── installing ───" (set_color -o cyan)"$argv"
    echo
    if [ -n "$distro" ]
      set filter ~/sync/dist/$distro/.apt-i.$distro
      if [ -f $filter ]
        set packages (echo $argv | sed 's/ /|/g')
        egrep -q "$packages" $filter; and set action i
      end
    end
    apt $action $argv ([ -f .install ]; and cat .install)
    [ -f .install ]; and /bin/rm -f .install
  end
  set error (cat ~/logs/apt.error | egrep 'dependencies:|E:')
  [ $error ]; and annotate "$error"
end

