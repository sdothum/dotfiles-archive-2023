function fish_right_prompt
  function remote
    if [ -n "$SSH_CLIENT" ]
      printf '%s%s ' (set_color -o yellow) (hostname)
    else
      printf ''
    end
  end

  function folder
    # printf '%s%s' (set_color yellow) (pwd | sed -re "s|^$HOME|~|" -e  's|([^/.])[^/]*/|\1/|g')
    set -l folders (pwd | sed -e "s|^$HOME|~|" -e 's|/|\t|g' | rev)
    set -l base (echo $folders | cut -f1,2 | rev)
    set -l parent (echo $folders | cut -f3- | rev | sed -r 's|([^\t.])[^\t]*\t*|\1/|g')
    printf '%s%s' (set_color yellow) (echo "/$parent$base" | sed -e 's|\t|/|g' -e 's|^//|/|' -e 's|^/~/~|~|' -e 's|^/~/|~/|')
  end

  function time
    date '+  %-I:%M %S'
  end

  function duration
    if test $CMD_DURATION
      if test $CMD_DURATION -gt 1000
        printf '  …%ss' (math "$CMD_DURATION / 1000")
      end
    end
  end

  # oddly, attempting a single formatted printf line doesn't display as expected
  # printf '%s %s%s %s%s' (gitstatus) (host) (dir) (time) (set_color normal)
  gitstatus
  echo '  '
  remote
  folder
  set_color 666
  # time
  duration
end
