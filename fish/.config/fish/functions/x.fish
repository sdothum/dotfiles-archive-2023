function x
  # refer to .xsession for corresponding window manager references
  switch "$argv[1]"
    case [1];                 echo ".. starting" (cat ~/.windowmanager) "window manager"
    case 2bwm 2b\*;           echo 2bwm > ~/.windowmanager
    case awesome aw\*;        echo awesome > ~/.windowmanager
    case bspwm bs\*;          echo bspwm > ~/.windowmanager
    case dwm dw;              echo dwm > ~/.windowmanager
    case goomwwm go\*;        echo goomwwm > ~/.windowmanager
    case herbstluftwm he\*;   echo herbstluftwm > ~/.windowmanager
    case i3;                  echo i3 > ~/.windowmanager
    case notion no\*;         echo notion > ~/.windowmanager
    case openbox op\*;        echo openbox > ~/.windowmanager
    case ratpoison ra\*;      echo ratpoison > ~/.windowmanager
    case spectrwm sp\*;       echo spectrwm > ~/.windowmanager
    case stumpwm st\*;        echo stumpwm > ~/.windowmanager
    case subtle su\*;         echo subtle > ~/.windowmanager
    case xfce4 xf\*;          echo xfce4 > ~/.windowmanager
                              # return to console instead
    case shell sh\*;          echo shell > ~/.windowmanager
    case wmii wm\*;           echo wmii > ~/.windowmanager
    case xmonad xm\*;         echo xmonad > ~/.windowmanager
    case '*';                 echo ".. x  'start  awesome  dwm  goomwwm  herbstluftwm  i3  notion  openbox  ratpoison  spectrwm  stumpwm  subtle  xfce4  shell  wmii  xmonad"; return
  end

  # called from init console
  if [ -z "$DISPLAY" -a (tty) = /dev/tty1 ]
    own-sync
    # return to login shell prompt
    [ (cat ~/.windowmanager) = shell ]; and return
    [ (cat ~/.windowmanager) = xmonad ]; and command cp -f ~/.config/terminator/config.xmonad ~/.config/terminator/config
    # "exec" exits directly to init fork (and login cycle)
    # !p startx; and begin; [ (hostname) = "luna" ]; and exec startx -- -layout Multihead; or exec startx --; end
    # multihead layout defined manually in xorg.conf
    !p startx; and startx --
  else
    # already in X11 session
    echo ".. window manager set to" (cat ~/.windowmanager) "for next xsession"
  end
end
