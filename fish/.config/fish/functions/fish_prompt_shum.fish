function fish_prompt --description 'Write out the prompt'

	set -g RCODE $status

  # console prompt hacks for the anal..
  if tty | grep -q tty
    set -g TTY tty
    set -g YELLOW '-o yellow'
    set -g ORANGE 'yellow'
    set -g RED    'red'
    set -g BLUE   'blue'
  else
    set -g TTY pts
    set -g YELLOW 'FC6'
    set -g ORANGE 'F60'
    set -g RED    'F00'
    set -g BLUE   '03F'
  end
  set -g BLINK (printf "\e[5m")
  set -g NOBLINK (printf "\e[25m")

  function glyph
    [ "$TTY" = tty ]
      and echo -n "$argv[2]"
      or echo -n "$argv[1]"
  end

  function space
    set_color normal
    echo -n ' '
  end

  function bgjobs
    if jobs -c | egrep -qv '^(Command|fasd)'
      set_color $BLUE
      glyph '⚙' 'o'
      set_color normal
    else
      space
    end
  end

  function root
    if [ "$USER" = root ]
      set_color $YELLOW
      echo -n $BLINK(glyph '⚡' 'z')$NOBLINK
      set_color normal
    else
      space
    end
  end

  function rcode
    if [ $RCODE -eq 0 ]
     space
    else
      set_color $RED
      glyph '✘' 'x'
      set_color normal
    end
  end

  function leader
    set_color $ORANGE
    if test "$fish_key_bindings" = "fish_vi_key_bindings"
      switch $fish_bind_mode
        case default
          echo -n ' ──_──  '
        case insert
          echo -n ' ─────  '
        case replace-one
          echo -n ' ──═──  '
        case visual
          echo -n ' ── ──  '
        case '*'
          echo -n ' ─ ? ─  '
      end
    else
      echo -n ' ─────  '
    end
    set_color normal
  end
  
  # taking a page out of the right hand prompt
  # printf '%s%s%s %s─────%s  ' (jobg) (root) (rcode) (set_color $ORANGE) (set_color normal)
  bgjobs
  root
  rcode
  leader

  # If commands runs >= 10 seconds, notify user on completion
  if test $CMD_DURATION
    if test $CMD_DURATION -gt (math "1000 * 10")
      set secs (math "$CMD_DURATION / 1000")
      notify low "$history[1]" "Returned $status, took $secs seconds"
    end
  end
end
