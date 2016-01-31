function p
  # 'or' a list of process names
  set list (echo $argv | tr ' ' '|' )
  # ps -ef --sort '-%cpu' | egrep -i "$list" 2>/dev/null | egrep -v egrep; 
  ps -ef --sort '-%cpu' | egrep -i "$list" 2>/dev/null | egrep -v ' grep '; 
end
