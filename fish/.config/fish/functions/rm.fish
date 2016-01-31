function rm
  # if --force
  if-opt f "$argv"; and begin
    echo "$argv" | sed 's/^\-[[:alnum:]]*//' | sed 's/ \-[[:alnum:]]*//g'
    if-yes "purge these files, no questions asked"; or return
  end
  command rm -i $argv; 
end
