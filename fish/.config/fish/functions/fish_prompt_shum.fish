function fish_prompt
  and set -g RCODE ok
  or set -g RCODE fail

  # console prompt hacks for the anal..
  if tty | grep -q tty
    set -g YELLOW '-o yellow'
    set -g ORANGE 'yellow'
    set -g RED    'red'
    set -g BLUE   'blue'
  else
    set -g YELLOW 'FC6'
    set -g ORANGE 'F60'
    set -g RED    'F00'
    set -g BLUE   '03F'
  end
  set -g BLINK (printf "\e[5m")
  set -g NOBLINK (printf "\e[25m")
  
  tty | grep -q tty; and set -g TTY tty; or set -g TTY pts
  function glyph
    [ "$TTY" = tty ]
    and printf '%s' "$argv[2]"
    or printf '%s' "$argv[1]"
  end

  function jobg
    jobs -c | egrep -qv '^(Command|fasd)'
    and printf '%s%s' (set_color $BLUE) (glyph '⚙' 'o')
    or printf '%s%s' (set_color normal) ' '
  end

  function root
    [ "$USER" = root ]
    and printf '%s%s' (set_color $YELLOW) $BLINK(glyph '⚡' 'z')$NOBLINK
    or printf '%s%s' (set_color normal) ' '
  end

  function rcode
    if [ "$RCODE" = ok ]
     printf '%s%s' (set_color normal) ' '
    else
      printf '%s%s' (set_color $RED) (glyph '✘' 'x')
    end
  end

  function leader
    set_color $ORANGE
    if test "$fish_key_bindings" = "fish_vi_key_bindings"
      switch $fish_bind_mode
        case default
          printf '%s' ' ──_──  '
        case '*'
          printf '%s' ' ─────  '
      end
    else
      printf '%s' ' ─────  '
    end
  end
  
  # taking a page out of the right hand prompt
  # printf '%s%s%s %s─────%s  ' (jobg) (root) (rcode) (set_color $ORANGE) (set_color normal)
  jobg
  root
  rcode
  leader
  set_color normal

  # If commands runs >= 10 seconds, notify user on completion
  if test $CMD_DURATION
    if test $CMD_DURATION -gt (math "1000 * 10")
      set secs (math "$CMD_DURATION / 1000")
      notify critical "$history[1]" "Returned $status, took $secs seconds"
    end
  end
end
