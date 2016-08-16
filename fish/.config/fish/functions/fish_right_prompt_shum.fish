function fish_right_prompt
  function host
    if [ -n "$SSH_CLIENT" ]
      printf '%s%s ' (set_color -o yellow) (hostname)
    else
      printf ''
    end
  end

  function dir
    printf '%s%s  ' (set_color yellow) (printf '%s' "$PWD" | sed -r 's|([^/.])[^/]*/|\1/|g')
  end

  function time
    printf '%s%s' (set_color 666) (date '+%-I:%M %S')
  end

  # oddly, attempting a single formatted printf line doesn't display as expected
  # printf '%s %s%s %s%s' (gitstatus) (host) (dir) (time) (set_color normal)
  gitstatus
  echo '  '
  host
  dir
  time
end
