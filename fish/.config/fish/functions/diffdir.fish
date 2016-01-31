function diffdir
  set dirlist .(random)
  ls -1 $argv[1] > $dirlist.$argv[1]
  ls -1 $argv[2] > $dirlist.$argv[2]
  diff $dirlist.$argv[1] $dirlist.$argv[2] | grep '^< ' | sed 's/^< //'
  command rm -f $dirlist.$argv[1] $dirlist.$argv[2]
end
