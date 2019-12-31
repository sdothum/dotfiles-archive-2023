function cp
  # if --force
  if-opt f "$argv"; or set confirm -i; and begin
    echo "$argv" | sed 's/^\-[[:alnum:]]*//' | sed 's/ \-[[:alnum:]]*//g' | sed 's/ \(.*\) \([^ ]*\)$/\1  ::  \2/'
    if-yes "copy these files, no questions asked"; or return
  end
  command cp $confirm --no-preserve=mode,ownership $argv; 
end
