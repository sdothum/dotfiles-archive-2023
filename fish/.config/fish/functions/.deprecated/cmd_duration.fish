function cmd_duration --description 'Displays the human readable command duration'
    set -l secs (math "$CMD_DURATION / 1000")
    set -l mins (math "$secs / 60")
    set -l hrs (math "$mins / 60")
    [ $hrs -gt 0 ]
      and printf '%sh ' $hrs
    [ $mins -gt 0 ]
      and printf '%sm ' $mins
    printf '%ss' (math "$secs - $mins * 60")
end
