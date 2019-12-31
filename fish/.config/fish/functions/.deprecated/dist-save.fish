function dist-save
  set root ~/sync/dist
  if [ (count $argv) -gt 0 ]
    [ (count $argv) -eq 2 ]; and set host ".$argv[2]"; or set host ''
    echo "... copying "(sudo cp -v $argv[1] $root/$argv[1]$host)
  end
end
