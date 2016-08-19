function fish_right_prompt
  if tty | grep -q tty
    set -g GREY 'red'
  else
    set -g GREY '666'
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
    if [ -n "$SSH_CLIENT" ]
      set_color -o yellow
      echo -n (hostname)' '
      set_color normal
    end
  end

  function folder
    set -l folders (pwd | sed -e "s|^$HOME|~|" -e 's|/|\t|g' | rev)
    set -l base (echo $folders | cut -f1,2 | rev)
    set -l parent (echo $folders | cut -f3- | rev | sed -r 's|([^\t.])[^\t]*\t*|\1/|g')
    set_color yellow
    # pwd | sed -re "s|^$HOME|~|" -e  's|([^/.])[^/]*/|\1/|g'
    echo -n "/$parent$base" | sed -e 's|\t|/|g' -e 's|^//|/|' -e 's|^/~/~|~|' -e 's|^/~/|~/|'
    set_color normal
  end

  function time
    set_color $GREY
    date '+  %-I:%M %S'
    set_color normal
  end

  function duration
    if test $CMD_DURATION
      if test $CMD_DURATION -gt 1000
        set_color $GREY
        echo -n '  …'(math "$CMD_DURATION / 1000")'s'
        set_color normal
      end
    end
  end

  gitstatus
  remote
  folder
  # time
  duration
end
