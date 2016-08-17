function fish_right_prompt
  function remote
    if [ -n "$SSH_CLIENT" ]
      printf '%s%s ' (set_color -o yellow) (hostname)
    else
      printf ''
    end
  end

  function folder
    printf '%s%s' (set_color yellow) (printf '%s' "$PWD" | sed -re "s|^$HOME|~|" -e  's|([^/.])[^/]*/|\1/|g')
  end

  function time
    date '+  %-I:%M %S'
  end

  function duration
    if test $CMD_DURATION
      if test $CMD_DURATION -gt 1000
        printf '  %sS' (math "$CMD_DURATION / 1000")
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
