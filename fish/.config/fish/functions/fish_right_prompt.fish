function fish_right_prompt --description 'Write out the right prompt'
  set -g HILIGHT 1

  if tty | grep -q tty
    set -g TTY tty
    set -g GREY   'red'
    set -g YELLOW 'green'
  else
    set -g TTY pts
    set -g GREY   '666'
    set -g YELLOW 'FC3'
  end

  function gitstatus
    if not set -q __fish_git_prompt_show_informative_status
      set -g __fish_git_prompt_show_informative_status 1
    end
    if not set -q __fish_git_prompt_hide_untrackedfiles
      set -g __fish_git_prompt_hide_untrackedfiles 1
    end

    if not set -q __fish_git_prompt_color_branch
      set -g __fish_git_prompt_color_branch magenta --bold
    end
    if not set -q __fish_git_prompt_showupstream
      set -g __fish_git_prompt_showupstream "informative"
    end
    if not set -q __fish_git_prompt_char_upstream_ahead
      set -g __fish_git_prompt_char_upstream_ahead "↑"
    end
    if not set -q __fish_git_prompt_char_upstream_behind
      set -g __fish_git_prompt_char_upstream_behind "↓"
    end
    if not set -q __fish_git_prompt_char_upstream_prefix
      set -g __fish_git_prompt_char_upstream_prefix ""
    end

    if not set -q __fish_git_prompt_char_stagedstate
      set -g __fish_git_prompt_char_stagedstate "●"
    end
    if not set -q __fish_git_prompt_char_dirtystate
      set -g __fish_git_prompt_char_dirtystate "✱"
    end
    if not set -q __fish_git_prompt_char_untrackedfiles
      set -g __fish_git_prompt_char_untrackedfiles "…"
    end
    if not set -q __fish_git_prompt_char_conflictedstate
      set -g __fish_git_prompt_char_conflictedstate "✖"
    end
    if not set -q __fish_git_prompt_char_cleanstate
      set -g __fish_git_prompt_char_cleanstate "✔"
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

    echo -n (__fish_git_prompt)'  '
    set_color normal
  end

  function remote
    if test -n "$SSH_CLIENT"
      set_color -o yellow
      echo -n (hostname)' '
      set_color normal
    end
  end

  function folder
    if test $HILIGHT -eq 1
      set -l folders (pwd | sed -e "s|^$HOME|~|" -e 's|/|\t|g' | rev)
      set -l base (echo $folders | cut -f1 | rev)
      set -l parent (echo $folders | cut -f2 | rev)
      set -l tree (echo $folders | cut -f3- | rev | sed -r 's|([^\t.])[^\t]*\t*|\1/|g')
      set_color yellow
      set -l path (echo "/$tree/$parent/"(set_color $YELLOW)"$base" | sed -e 's|///*|/|' -e 's|^/~/.*~|~|' -e 's|^/~/|~/|')
      test "$path" = '~' 
        and set -l path (set_color $YELLOW)~
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

  function cmd_duration
    set -l secs (math "$CMD_DURATION / 1000")
    set -l mins (math "$secs / 60")
    set -l hrs (math "$mins / 60")
    if test $hrs -gt 0 
      printf '%sh ' $hrs
      set mins (math "$mins - $hrs * 60")
      set secs (math "$secs - $hrs * 3600")
    end
    test $hrs -gt 0 -o $mins -gt 0 
      and printf '%sm ' $mins
    printf '%ss' (math "$secs - $mins * 60")
  end

  function duration
    if test $CMD_DURATION
      if test $CMD_DURATION -gt 1000
        set_color $GREY
        echo -n '  ⇣'(cmd_duration $CMD_DURATION)
        set_color normal
        test "$TTY" = pts 
          and test $CMD_DURATION -gt (math "1000 * 10")
            and notify low "$history[1]" "Returned $status, took "(cmd_duration)
        return (true)
      end
    end
    false
  end

  function time
    set_color $GREY
    date '+  %-I:%M %S'
    set_color normal
  end

  gitstatus
  remote
  folder
  duration
    or if test "$TTY" = tty 
      time
    end
end
