
# additional user fish config startup

if [ $USER != root ]
  # init or x session?
  if [ -z "$DISPLAY" -a (tty) = /dev/tty1 ]
    # set numdisks (ll /dev/sd*1 | wc -l)
    # if [ $numdisks -gt 2 -a ! -f /sbin/zfs ]
    #   #if-yes "removing ~/.startx.. inject zfs into new kernel"; and eval ~/sync/dist/sid/install-zfs
    #   [ -f ~/.startx ]; and rm -f ~/.startx 2>/dev/null
    # end
    # sudo zpool status > ~/zpool.status
    # cat ~/zpool.status
    # if [ $HOST = luna ]
    #   # delay required because systemd is too fast to login! :-)
    #   #[ -d /data/depot ]; or begin; sleep 5s; zpool mount; end
    #   [ -d /data/depot ]; or zpool mount
    # end
    # adsuck dns filter in place
    grep -q nameserver /etc/resolv.conf; or begin
      sudo sed -i '$a \
nameserver 127.0.0.1' /etc/resolv.conf
      grep -q nohook /etc/dhcpcd.conf; or sudo sed -i '$a \
nohook resolv.conf' /etc/dhcpcd.conf
    end
    if [ (hostname) = luna ]
      pidof wpa_supplicant; or begin
        ping -q -c1 10.1.0.1; and sleep 3s; and wicd-cli -c -y -n (wicd-cli -ly | grep Ravens | cut -f1)
      end
    end
    echo
    df

    # autostart x11?
    [ -f ~/.startx ]; and x
  else
    # kill that pesky dangling multiple screensaver that sometimes spawns
    #screentrimmer 0 >/dev/null
    # restore compositing state as required
    video

    # Current project directory
    [ -f ~/.logindir ]; and cd (cat ~/.logindir) 2>/dev/null
    # A little humour to start the day...
    [ -f /usr/games/fortune ]; and /usr/games/fortune; or begin;
      [ -f /usr/bin/fortune ]; and /usr/bin/fortune
    end
    raise Conky
  end

  #setrvm
end
