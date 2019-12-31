function inline
  set inline .(random)
  # "inline file command" operation on same file
  [ (count $argv) -lt 2 ]; and echo '.. inline file "command"'; and return
  [ ! -f "$argv[1]" ]; and echo ".. file \"$argv[1]\" not found"; and return
  command mv "$argv[1]" "$inline.$argv[1]"
  trace 1 [inline] $argv[(seq 2 (count $argv))] $argv[1]
  eval $argv[(seq 2 (count $argv))] <$inline.$argv[1] > $argv[1]
  command rm -f $inline.$argv[1]
end
