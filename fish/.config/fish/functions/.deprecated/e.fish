function e
  [ (stat -c \%U $argv[1]) = $USER ]; and emacs --no-splash $argv &; or sudo emacs --no-splash $argv &; 
end
