function video
  # manually manage xscreensaver and compositing for video media
  # video <player>
  set players gnome-mplayer minitube smplayer umplayer vlc xbmc

  function display-off
    trace 1 display-off
    # kill dangling screensavers
    # killall (cat ~/.screensaver) 2>/dev/null
    killall xscreensaver 2>/dev/null
    # killall xwinwrap 2>/dev/null
    xset s off -dpms
    # if compositing causes choppy video
    killall compton 2>/dev/null
  end

  function display-on
    trace 1 display-on
    !p compton; and compton
    xset s on +dpms
    !p "xscreensaver.*-no-splash"; and [ -f /usr/bin/xscreensaver ]; and xscreensaver -no-splash &
    # !p xwinwrap; and bgscreensaver &
  end

  switch "$argv[1]"
    case [1];       for i in $players
                      set pids (pidof $i) (pidof $i.bin)
                      trace 1 "pids = $pids"
                      if [ "$pids." != "." ]
                        display-off
                        return
                      end
                    end
                    display-on
    case $players;  !p /usr/bin/$argv[1]; and !p $argv[1].bin; and begin
                      display-off
                      if [ (count $argv) -gt 1 ]
                        [ $argv[1] = umplayer ]
                          and set media (escape $argv[(seq 2 (count $argv))])
                          or set media (uri $argv[(seq 2 (count $argv))])
                      end
                      eval /usr/bin/$argv[1] "$media"
                      video
                    end
    case off;       display-off
    case on;        display-on
    case '*';       echo ".. video 'auto | off | on"
  end
end
