function pwdstamp
  # rename current file/folder with parent (directory) name
  [ (count $argv) -eq 0 ]; and echo "pwdstamp filename"; and return

  set parent (dirname $argv)
  set file (basename $parent)

  # if renaming file, preserve extension
  [ -f "$argv" ]; and set ext .(echo "$argv" | awk -F . '{print $NF}')

  echo "$argv :: $parent/$file$ext"
  command mv "$argv" "$parent/$file$ext"
end
