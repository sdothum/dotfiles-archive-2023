function etc-config
  if [ (count $argv) -gt 0 ]
    original $argv[1]
    [ (count $argv) -eq 2 ]; and set host ".$argv[2]"; or set host ''
    sudo cp -v --no-preserve=mode,ownership /home/shum/.config$argv[1]$host $argv[1]
    log " ... copying /home/shum/.config$argv[1]$host  -=>>  $argv[1]"
    annotate "check configuration file $argv[1]"
  end
end
