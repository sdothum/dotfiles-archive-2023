function f
  # echo find $argv[1] -name $argv[2]; command sudo find $argv[1] -name $argv[2] 2>/dev/null; 
  sudo find $argv[1] -iname $argv[2] $argv[(seq 3 (count $argv))]
end
