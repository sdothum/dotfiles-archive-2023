function fish_right_prompt --description 'Write out the right prompt'
  set -g POSTFIX 0
  # set -g VERBOSE 0

  if test $TERM = "linux"
    set -g _info   'red'
    set -g _loc    'green'
  else
    # set -g _info '666'
    # set -g _loc  'FC3'
    # quantum colour palette
    set -g _info   'aebbc5'  # grey
    set -g _loc    'd5b875'  # yellow
  end

  function glyph
    test $TERM = "linux"
      and echo -n "$argv[2]"
      or echo -n "$argv[1]"
  end

  function gitstatus
    if not set -q __fish_git_prompt_show_informative_status
      set -g __fish_git_prompt_show_informative_status '↑'
    end
    if not set -q __fish_git_prompt_hide_untrackedfiles
      set -g __fish_git_prompt_hide_untrackedfiles '↑'
    end

    if not set -q __fish_git_prompt_color_branch
      set -g __fish_git_prompt_color_branch magenta --bold
    end
    if not set -q __fish_git_prompt_showupstream
      set -g __fish_git_prompt_showupstream 'informative'
    end
    if not set -q __fish_git_prompt_char_upstream_ahead
      set -g __fish_git_prompt_char_upstream_ahead '↑'
    end
    if not set -q __fish_git_prompt_char_upstream_behind
      set -g __fish_git_prompt_char_upstream_behind '↓'
    end
    if not set -q __fish_git_prompt_char_upstream_prefix
      set -g __fish_git_prompt_char_upstream_prefix ''
    end

    if not set -q __fish_git_prompt_char_stagedstate
      set -g __fish_git_prompt_char_stagedstate (glyph '●' '■')
    end
    if not set -q __fish_git_prompt_char_dirtystate
      set -g __fish_git_prompt_char_dirtystate (glyph '*' '*')
    end
    if not set -q __fish_git_prompt_char_untrackedfiles
      set -g __fish_git_prompt_char_untrackedfiles '…'
    end
    if not set -q __fish_git_prompt_char_conflictedstate
      set -g __fish_git_prompt_char_conflictedstate (glyph '✖' 'x')
    end
    if not set -q __fish_git_prompt_char_cleanstate
      set -g __fish_git_prompt_char_cleanstate (glyph '✔' '=')
    end

    if not set -q __fish_git_prompt_color_dirtystate
      set -g __fish_git_prompt_color_dirtystate blue
    end
    if not set -q __fish_git_prompt_color_stagedstate
      set -g __fish_git_prompt_color_stagedstate yellow
    end
    if not set -q __fish_git_prompt_color_invalidstate
      set -g __fish_git_prompt_color_invalidstate red
    end
    if not set -q __fish_git_prompt_color_untrackedfiles
      set -g __fish_git_prompt_color_untrackedfiles $fish_color_normal
    end
    if not set -q __fish_git_prompt_color_cleanstate
      set -g __fish_git_prompt_color_cleanstate green --bold
    end

    set -l git (__fish_git_prompt)
    test -z "$git"
      and return
    echo -n " $git"
    set_color normal
  end

  function remote
    echo -n '  '
    if test -n "$SSH_CLIENT"
      set_color $_loc
      echo -n (hostname)
      set_color $_info
      echo ':'
      set_color normal
    end
  end

  function folder
    set -l folders (pwd | sed "s,^$HOME,~,; s,/,\t,g" | rev)
    set -l base (echo $folders | cut -f1,2 | rev)
    set -l parent (echo $folders | cut -f3- | rev | sed -r 's,([^\t.])[^\t]*\t*,\1/,g')
    set_color yellow
    echo -n "/$parent$base" | sed -r "s,\t,/,g; s,//,/,; s,^/(.*~|\.\.),~,"
    set_color normal
  end

  function duration
    if test $CMD_DURATION
      if test 0$CMD_DURATION -gt 1000
        test 0$POSTFIX -eq 1
          and set_color $_info
          and glyph '  ^' '  ^'
        # echo -n (cmd_duration)
        echo -n (chrono -time 0 "$CMD_DURATION / 1000")
        set_color normal
        test $TERM != "linux"
          and test 0$CMD_DURATION -gt (math "1000 * 10")
            # and notify 3 low "$history[1]" "Returned $status, took "(cmd_duration)
            and notify 3 low "$history[1]" "Returned $status, took "(chrono -time 0 "$CMD_DURATION / 1000")
        test 0$POSTFIX -eq 1
          or begin
            set_color $_info
            # glyph '░' '░'
            glyph '⇣' '_'
          end
        return (true)
      end
    end
    false
  end

  function timequery
    set_color $_info
    date '+  %-I:%M %S'
    set_color normal
  end

  function timer
    duration
      or if test $TERM = "linux"
        timequery
      end
  end

  test 0$POSTFIX -eq 1
    or timer
  cpu arm
    or gitstatus
  remote
  folder
  test 0$POSTFIX -eq 1
    and timer
end
