function save
  set file "$argv[1]"
  if [ -f "$file.save" ]
    if-yes "overwrite \"$file.save\""; or return 1
  end
  [ -f "$file" ]; and echo "... copying "(sudo cp -Rv "$file" "$file.save"); and return 0
  return -1
end
