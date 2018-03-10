function fish_prompt --description 'Write out the prompt'

	set -g RCODE $status

  # console prompt hacks for the anal..
  if test $TERM = "linux"
    set -g YELLOW   'normal'
    set -g ORANGE   'red'
    set -g RED      'red'
    set -g BLUE     'blue'
  else
    # set -g YELLOW 'FC6'
    # set -g ORANGE 'F60'
    # set -g RED    'F00'
    # set -g BLUE   '03F'
    # quantum colour palette
    set -g YELLOW   'd5b875'
    set -g ORANGE   'd7956e'
    set -g RED      'dd7186'
    set -g BLUE     '70ace5'
  end
  set -g BLINK (printf "\e[5m")
  set -g NOBLINK (printf "\e[25m")

  function glyph
    if test $TERM = "linux"
      echo -n "$argv[2]"
    else
      echo -n "$argv[1]"
    end
    set_color normal
  end

  function space
    set_color normal
    echo -n ' '
  end

  function bgjobs
    if jobs -c | egrep -qv '^(Command|fasd|autojump)'
      set_color $BLUE
      glyph '⊛' 'o'
    else
      space
    end
  end

  function root
    if test "$USER" = root
      set_color $YELLOW
      # ⚡ takes up 2 byte positions ??
      # echo -n $BLINK(glyph '⚡' 'z')$NOBLINK
      echo -n $BLINK(glyph '❱' 'z')$NOBLINK
    else
      space
    end
  end

  function rcode
    if test 0$RCODE -eq 0
     space
    else
      set_color $RED
      glyph '✖' 'x'
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
  rcode
  root
  leader
end
