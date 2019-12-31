function lsof
  [ (count $argv) -eq 0 ]; and set process plex; or set process "$argv[1]"
  if [ (count $argv) -le 1 ]
    [ $process = plex ]; and set file videos; or set file '.*'
  else
    set file "$argv[2]"
  end
  echo "$process /$file/"
  for i in (p "$process" | awk '{print $2}' | uniq)
    [ "$file" = ".*" ]; and sudo lsof -P -n -p $i; or sudo lsof -P -n -p $i | a "$file"
  end
end
