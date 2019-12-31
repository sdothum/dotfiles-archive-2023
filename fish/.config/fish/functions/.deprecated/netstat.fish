function netstat
  echo netstat -tnp $argv; command sudo netstat -tnp $argv; 
end
