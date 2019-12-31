function restart
  function stop_start
    stop $argv
    start $argv
    status $argv.service
  end

  switch "$argv[1]"
    case plex;      stop_start plexmediaserver
    case proxy;     echo ".. polipo"; stop_start polipo
                    echo ".. privoxy"; stop_start privoxy
                    echo ".. squid"; stop_start squid
    case sshd;      stop_start restart ssh
    case synapse;   zeitgeist-daemon --quit; zeitgeist-daemon &
    case web www;   stop_start nginx
    case '*';       [ -z $argv ]; and echo ".. restart <init.d process>"; and return
                    if echo $argv | grep -q '\.sh'
                      sudo /etc/init.d/$argv stop
                      sudo /etc/init.d/$argv start
                      return
                    end
                    stop_start $argv
  end
end
