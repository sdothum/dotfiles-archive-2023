function fish_prompt
  and set -g __returncode green; or set -g __returncode red
  tty | grep -q tty; and set -g __tty tty; or set -g __tty pts

  function hilight
    set_color normal; set_color $__returncode; echo -n "$argv"; set_color normal
  end

  function glyph
    [ (count $argv) -eq 1 ]; and hilight "$argv"; and return
    [ $__tty = tty ]; and hilight "$argv[1]"; or hilight "$argv[2]"
  end

  function separator
    set_color green; echo -n "$argv"; set_color normal
  end

  # manually moving the cursor up 1 row solves the double prompt behaviour..
  # unfortunately, it also overwrites the last line of the command output
  # tput cuu1
  glyph '┌─'; separator '[ '
  set date (date '+%a %-I:%M %S%P')
  hilight (set_color -o)"$date"

  if [ -f /usr/bin/acpi ]
    if [ (acpi -a 2>/dev/null | grep off) ]
      set battery ' Battery'(acpi -b|cut -d',' -f2)
      set_color -o red; echo -n "$battery"
    end
  end
  separator ' ]'; glyph '──'; separator '[ '

  [ $USER = root ]; and set_color -o red; or set_color -o cyan; echo -n "$USER "
  set hostname (hostname)
  if [ -n "$SSH_CLIENT" ]
    # set_color -o red; and echo -n "$hostname"
    set_color -ou yellow; echo -n "$hostname"; set_color normal; echo -n :
  else
    set hostname
  end

  # split location if column width exceeded
  set directory (pwd | sed "s,$HOME,~,")
  [ (tput cols) -le 62 ]; and begin
    set margin (solve (tput cols) - (echo "┌─[ $date$battery ]─[ $USER $hostname:$directory ]" | wc -c))
    [ $margin -le 0 ]; and begin
      echo; set_color normal; glyph '│ '
    end
  end
  # [ $USER = root ]; and set_color -o red; or set_color -o yellow; echo -n "$directory"
  set_color -o yellow; echo -n "$directory"
  set_color normal; separator ' ]'; echo

  for job in (jobs)
    set_color normal; glyph '│ '
    set_color -o cyan; echo $job
  end

  set_color normal; glyph '└────── ' '└─────╼ '
  set -e __returncode; set -e __tty
end
