function uri
  echo file://(echo $argv | sed 's/ /%20/g')
end
