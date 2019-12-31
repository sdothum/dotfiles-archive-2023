function em
  [ (stat -c \%U $argv[1]) = $USER ]; and emacs -nw $argv &; or sudo emacs -nw $argv &; 
end
