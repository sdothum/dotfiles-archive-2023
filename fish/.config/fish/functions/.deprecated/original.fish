function original
  set file "$argv[1]"
  if [ "$argv[1]" = "-f" ]; set file "$argv[2]"; sudo rm -f "$file.original" 2>/dev/null; end
  [ -f "$file.original" ]; and return 1
  [ -f "$file" ]; and echo "... copying "(sudo cp -Rv "$file" "$file.original"); and return 0
  return -1
end
