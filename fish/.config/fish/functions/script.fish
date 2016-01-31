function script
  set --export SHELL /bin/bash
  [ -f ~/typescript ]; and if-yes "create fresh typescript log"; and begin;
    echo ".. saving previous to $HOME/logs/typescript.log"
    [ -d ~/logs ]; or mkdir -v ~/logs
    cat ~/typescript >> ~/logs/typescript.log
    cat /dev/null > ~/typescript
  end
  command script -a ~/typescript
end
