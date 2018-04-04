function fish_prompt --description 'Write out the prompt'

	set -g RCODE $status

  # console prompt hacks for the anal..
  if test $TERM = "linux"
    set -g _warning   'normal'
    set -g _leader    'red'
    set -g _error     'red'
    set -g _bgjob     'blue'
  else
    # set -g _warning 'FC6'
    # set -g _leader  'F60'
    # set -g _error   'F00'
    # set -g _bgjob   '03F'
    # quantum colour palette
    set -g _warning   'd5b875'  # yellow
    set -g _leader    'd7956e'  # orange
    set -g _error     'dd7186'  # red
    set -g _bgjob     '70ace5'  # blue
  end
  # set -g BLINK (printf "\e[5m")
  # set -g NOBLINK (printf "\e[25m")

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
      set_color $_bgjob
      glyph '◔' '+'
    else
      space
    end
  end

  function root
    if test "$USER" = root
      set_color $_warning
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
      set_color $_error
      glyph '✖' 'x'
    end
  end

  function leader
    set_color $_leader
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
