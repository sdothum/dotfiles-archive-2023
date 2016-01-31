function pid
  p $argv | /usr/bin/awk '{print $2}'
end

