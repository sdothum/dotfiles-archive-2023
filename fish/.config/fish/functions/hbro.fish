function hbro
  set -x HTTP_PROXY
  [ -z "$argv" ]; and command hbro http://thedarnedestthing &; or command hbro $argv &
end
