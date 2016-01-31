function escape
  echo "$argv" | sed 's/\([ &;(){}]\)/\\\\\1/g'
end
