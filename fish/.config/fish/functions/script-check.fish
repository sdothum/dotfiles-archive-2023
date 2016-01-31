function script-check
  if [ (pidof script | wc -l) -eq 0 ]
    echo '... run "script" to log stdout messages'
    exit
  end
end
