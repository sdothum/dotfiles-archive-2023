function dist-config
  set root ~/sync/dist
  if [ (count $argv) -gt 0 ]
    original $argv[1]
    [ (count $argv) -eq 2 ]; and set host ".$argv[2]"; or set host ''
    echo "... copying "(sudo cp -v --no-preserve=mode,ownership $root$argv[1]$host $argv[1])
    annotate "check configuration file $argv[1]"
  end
end
