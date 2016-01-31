function ted
  set timestamp (mktemp)
  # file expression required vs expanded file list (to handle filenames with blanks)
  [ (count $argv) -lt 2 ]; and echo '.. ted "sed options" "files"'; and return
  set queue $argv[(seq 2 (count $argv))]
  for file in $queue
    touch -r "$file" $timestamp
    sed -i "$argv[1]" $file
    # restore timestamp to retain chronological order
    touch -r $timestamp "$file"
  end
  command rm -f $timestamp
end
