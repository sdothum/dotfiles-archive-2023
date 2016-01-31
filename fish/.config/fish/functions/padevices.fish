function padevices
  underline Sources
  pactl list | grep -A2 'Source #' | grep 'Name: ' | cut -d' ' -f2
  underline Sinks
  pactl list | grep -A2 'Sink #' | grep 'Name: ' | cut -d' ' -f2
end
