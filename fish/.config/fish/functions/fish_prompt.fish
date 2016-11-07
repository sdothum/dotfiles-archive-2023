function fish_prompt --description 'Write out the prompt'

	set -g RCODE $status

  # console prompt hacks for the anal..
  if test $TERM = "linux"
    set -g YELLOW 'normal'
    set -g ORANGE 'red'
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

  function glyph
    test $TERM = "linux"
      and echo -n "$argv[2]"
      or echo -n "$argv[1]"
  end

  function space
    set_color normal
    echo -n ' '
  end

  function bgjobs
    if jobs -c | egrep -qv '^(Command|fasd|autojump)'
      set_color $BLUE
      glyph '⊛' 'o'
      set_color normal
    else
      space
    end
  end

  function root
    if test "$USER" = root
      set_color $YELLOW
      echo -n $BLINK(glyph '⚡' 'z')$NOBLINK
      set_color normal
    else
      space
    end
  end

  function rcode
    if test $RCODE -eq 0
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
          echo -n ' ──━──  '
        case '*'
          echo -n ' ─ ? ─  '
      end
    else
      echo -n ' ─────  '
    end
    set_color normal
  end

  bgjobs
  root
  rcode
  leader
end
