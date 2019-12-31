function pacman
  set shell /bin/sh
  # default (no options) to package query
  if-opt S $argv; and set notS false; or set notS true
  if [ (count $argv) -eq 1 -a $notS = true ]
    set action -Qs
    set package $argv
  else
    # no dash option shorthand for noconfirm :-)
    echo "$argv[1]" | grep -q '-'; and set action $argv[1]; or set action -$argv[1] --noconfirm
    set package $argv[(seq 2 (count $argv))]
  end
  sudo pacman $action --color auto $package
end
