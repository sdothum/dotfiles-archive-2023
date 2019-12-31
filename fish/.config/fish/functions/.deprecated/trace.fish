function trace
  # set or touch ~/.tracelevel to create session persistant tracelevel
  # "trace [0-9]" set global tracelevel, defaults to ~/.tracelevel, else 0
  # "trace [1-9] message"
  # "trace message" == "trace 0 message" which always echos
  # "trace" show current tracelevel
  if [ "$tracelevel" = "" ]
    [ -f ~/.tracelevel ]; and set --global tracelevel (cat ~/.tracelevel); or set --global tracelevel 0
  end
  # seq command causes string prefix iteration
  # [ $argv[1] -le $tracelevel ]; and echo (set_color red)"$argv[1]> "(set_color green)(date '+-%-I:%M.%S%P- ')(set_color normal)$argv[(seq 2 (count $argv))]
  [ (count $argv) -eq 0 ]; and echo "tracelevel := $tracelevel"; and return
  if [ (echo $argv[1] | grep '^[0-9]$') ]
    if [ (count $argv) -eq 1 ]
      set --global tracelevel $argv
      [ -f ~/.tracelevel ]; and echo $argv > ~/.tracelevel
      trace
      return
    end
    set level $argv[1]
    set message $argv[(seq 2 (count $argv))]
  else
    set level 0
    set message (set_color -o white)$argv
  end
  if [ $level -le $tracelevel ]
    log -n (set_color green)(date '+[ %a %-I:%M %S%P ] ')
    [ $level -gt 0 ]; and log -n (set_color red)"$level> "
    # message passed may contain set_color commands, so reset at end
    log (set_color normal)"$message"(set_color normal)
  end
end
