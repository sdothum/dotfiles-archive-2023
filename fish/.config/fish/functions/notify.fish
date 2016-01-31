function notify
  # for loop shorter than switch statement :-)
  for i in (seq (count $argv))
    set message $message \"$argv[$i]\"
  end
  # only accepts 2 arguments: "summary", "body"
  sudo notify-send --icon=dialog-information $message &
end
