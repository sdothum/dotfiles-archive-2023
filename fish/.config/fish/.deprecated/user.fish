# additional user fish config startup 

if [ $USER != root ]
    # init or x session?
    if [ -z "$DISPLAY" -a (tty) = /dev/tty1 ]
        set numdisks (ll /dev/sd*1 | wc -l)
        if [ $numdisks -gt 2 -a ! -f /sbin/zfs ]
            if-yes "removing ~/.startx.. inject zfs into new kernel"; and ~/bin/install/install-zfs
            rm -f ~/.startx 2>/dev/null
        end
        sudo zpool status

        # autostart x11?
        [ -f ~/.startx ]; and x
    else
        # kill that pesky dangling multiple screensaver that sometimes spawns
        screentrimmer 0 >/dev/null
        # restore compositing state as required
        video

        # Current project directory
        [ -f ~/.logindir ]; and cd (cat ~/.logindir) 2>/dev/null
        # A little humour to start the day...
        /usr/games/fortune

        rvm 1.9
    end
end

