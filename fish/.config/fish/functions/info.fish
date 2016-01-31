function info
  echo "PWD:  $PWD"
  echo "ARGS: $argv"
  if [ -f "$argv" ]
    ls -l $argv
    underline
    head $argv
    echo "      ."
    echo "      ."
    echo "      ."
    underline
  end
  echo "..press return to continue"
  read
end
