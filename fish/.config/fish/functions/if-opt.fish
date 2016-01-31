function if-opt
  # if-opt option arguments e.g. if-opt S $argv
  echo $argv[(seq 2 (count $argv))] | grep -q " *\-[[:alnum:]]*$argv[1]"
end
