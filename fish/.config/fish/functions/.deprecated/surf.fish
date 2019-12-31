function surf
  set -x HTTP_PROXY
  [ -z "$argv" ]; and command surf http://thedarnedestthing &; or command surf $argv &
end
