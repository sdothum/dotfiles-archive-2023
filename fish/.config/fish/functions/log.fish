function log
  [ "$logfile" = "" ]; and set logfile ~/logs/session.log
  # use -n option to suppress line break
  echo $argv >> $logfile
  echo $argv
end

