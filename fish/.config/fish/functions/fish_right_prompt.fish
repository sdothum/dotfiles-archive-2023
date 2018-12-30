function fish_right_prompt --description 'Write out the right prompt'
  set -g HILIGHT 1
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
      set -g __fish_git_prompt_color_branch magenta --bold:::
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
    if test 0$HILIGHT -eq 1
      set -l folders (pwd | sed -e "s|^$HOME|~|" -e 's|/|\t|g' | rev)
      set -l base (echo $folders | cut -f1 | rev)
      set -l parent (echo $folders | cut -f2 | rev)
      set -l tree (echo $folders | cut -f3- | rev | sed -re 's|([^\t.])[^\t]*\t*|\1/|g' -e 's|\t||g')
      set_color yellow
      set -l path (echo "/$tree/$parent/"(set_color $_loc)"$base" | sed -e 's|///*|/|' -e 's|^/~/.*~|~|' -e 's|^/~/|~/|')
      test "$path" = '~'
        and set -l path (set_color $_loc)~
      echo -n $path
    else
      set -l folders (pwd | sed -e "s|^$HOME|~|" -e 's|/|\t|g' | rev)
      set -l base (echo $folders | cut -f1,2 | rev)
      set -l parent (echo $folders | cut -f3- | rev | sed -r 's|([^\t.])[^\t]*\t*|\1/|g')
      set_color yellow
      echo -n "/$parent$base" | sed -e 's|\t|/|g' -e 's|//|/|' -e 's|^/~/~|~|' -e 's|^/~/|~/|'
    end
    set_color normal
  end

  # function cmd_duration
  #   set -l secs (math "$CMD_DURATION / 1000")
  #   set -l mins (math "$secs / 60")
  #   set -l hrs (math "$mins / 60")
  #   if test 0$hrs -gt 0
  #     if test $VERBOSE -gt 0
  #       printf '%sh ' $hrs
  #     else
  #       printf '%s ' $hrs
  #     end
  #     set mins (math "$mins - $hrs * 60")
  #     set secs (math "$secs - $hrs * 3600")
  #   end
  #   if test $VERBOSE -gt 0
  #     test 0$hrs -gt 0 -o $mins -gt 0
  #       and printf '%sm ' $mins
  #     printf '%ss' (math "$secs - $mins * 60")
  #   else
  #     test 0$hrs -gt 0 -o $mins -gt 0
  #       and printf '%s' $mins
  #       or printf '0'
  #     printf "'%s" (math "$secs - $mins * 60")
  #   end
  # end

  function duration
    if test $CMD_DURATION
      if test 0$CMD_DURATION -gt 1000
        test 0$POSTFIX -eq 1
          and set_color $_info
          and glyph '  ^' '  ^'
        # echo -n (cmd_duration)
        echo -n (chrono 0 "$CMD_DURATION / 1000")
        set_color normal
        test $TERM != "linux"
          and test 0$CMD_DURATION -gt (math "1000 * 10")
            # and notify 3 low "$history[1]" "Returned $status, took "(cmd_duration)
            and notify 3 low "$history[1]" "Returned $status, took "(chrono 0 "$CMD_DURATION / 1000")
        test 0$POSTFIX -eq 1
          or begin
            set_color $_info
            glyph '░' '░'
          end
        return (true)
      end
    end
    false
  end

  function time
    set_color $_info
    date '+  %-I:%M %S'
    set_color normal
  end

  function timer
    duration
      or if test $TERM = "linux"
        time
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
