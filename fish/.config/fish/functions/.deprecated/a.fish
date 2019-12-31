function a
  # sudo ack-grep --color --color-filename='bold green' --color-match='bold red' --ignore-case $argv; 
  sudo /usr/bin/vendor_perl/ack --color --color-filename='bold green' --color-match='bold red' --ignore-case $argv; 
end
