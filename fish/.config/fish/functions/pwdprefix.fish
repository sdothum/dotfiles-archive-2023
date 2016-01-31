function pwdprefix
  # rename filename (without extension) to directory name

  # rename files with parent (directory) name
  [ (count $argv) -eq 0 ]; and echo "pwdprefix filename [ directory ]"; and return

  # thunar custom action can pass file and directory names
  [ (count $argv) -eq 1 ]; and set parent (basename $PWD); or set parent (basename $argv[2])

  # set prefix
  [ -f "$argv[1]" ]; and set prefix (echo "$argv[1]" | sed 's/\(.*\)\..*/\1/')

  echo "$prefix => $parent"
  cd $parent
  for i in $prefix*
    set suffix (echo "$i" | sed "s/^$prefix//")
    #echo mv -i $i $parent$suffix
    command mv -i $i $parent$suffix
  end
end
