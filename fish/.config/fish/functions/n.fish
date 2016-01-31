function n
  [ (stat -c \%U $argv[1]) = $USER ]; and nano $argv &; or sudo nano $argv &; 
end
