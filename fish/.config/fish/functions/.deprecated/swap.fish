function swap
  set swap .(random)
  if [ (count $argv) -eq 2 ]
    if [ -f "$argv[1]" -a -f "$argv[2]" -o -d "$argv[1]" -a -d "$argv[2]" ]
      command mv "$argv[1]" "$swap.$argv[1]"
      command mv "$argv[2]" "$argv[1]"
      command mv "$swap.$argv[1]" "$argv[2]"
      echo ".. $argv[1] =>> <<= $argv[2]"
      return
    end
  end
  echo ".. swap file1 file2"
end
