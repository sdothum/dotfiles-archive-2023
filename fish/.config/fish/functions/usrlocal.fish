function usrlocal
  if [ (count $argv) -gt 0 -a -d /usr/local/bin/$1 ]
    sudo rm -Rf /usr/local/bin/$1
    sudo mv -f ~/downloads/$1 /usr/local/bin/
    cd /usr/local/bin/; echo $1; ll $1
  end
end
