-- | #!/usr/bin/env ruby
-- | encoding: UTF-8
-- | my default x11 session startup apps

import System.Directory
import System.Posix.Unistd
import System.Posix.Env (getEnv)
import XMonad

-- | session restart, kill prior session apps
killall x = spawn $ "sudo killall " ++ x ++ " 2>/dev/null"

sessionApps =
    [ "artha"
    , "autokey"
    , "clipit"
    , "dropbox"
    -- , "gpe-clock"
    , "imapfilter"
    , "launchy"
    , "ncmpcpp"
    , "newsbeuter"
    , "nm-applet"
    , "nzbget"
    , "offlineimap"
    , "pyradio"
    , "sup"
    , "synapse"
    , "trayer"
    , "trayion"
    , "xfce-power-manager"
    ]

-- | kill unique processes
killps x = spawn $ "kill -9 $(ps -ef | grep \"" ++ x ++ "\" | grep -v grep | awk '{print $2;}')"

uniqueProcesses =
    [ -- can't killall conky 'cause statusbar with conky already started (notion)
      "conky -p.* -q"
    , "fish.*imap"
    , "fish.*sup"
    , "fish.*todo"
    -- , "python.*CouchPotato.py --daemon"
    -- , "python.*Headphones.py --daemon"
    , "python.*SickBeard.py --daemon"
    , "python.*turses"
    -- , "tail.*imapfilter.log"
    , "slrn"
    , "xbmc"
    ]

-- | launch application process
launch _ (env, command) = spawn command
  where
    env = Common
launch a (env, command) = spawn command
  where
    env = a
launch _ (_, _)         = spawn "true"

-- | common session initialization
serverApps =
    [ -- remove any dangling lock files
      (Common,  "rm -f ~/.msmtpqueue/.lock")
    , (Common,  "rm -f ~/.nzbget.paused")
    , (Common,  "rm -f ~/.session/rtorrent.lock")
    , (Common,  "rm -f ~/.sup/lock")
    , (Common,  "rm -f ~/.sup/xapian/flintlock")
    -- session initialization
    , (Common,  "eval $(gnome-keyring-daemon -s --components=pkcs11,secrets,ssh,gpg)")
    , (Common,  "lxsession")
    , (Common,  "synclient VertEdgeScroll=1")
    , (Common,  "synclient TapButton1=1")
    , (Common,  "xmodmap /etc/X11/Xmodmap")
    , (Desktop, "xrandr --output DVI-1 --primary --mode 2560x1600 --rate 60 --right-of DVI-0")
    -- restart some daemons
    , (Common,  "sudo /etc/init.d/danted restart")
    , (Common,  "sudo /etc/init.d/nginx restart")
    , (Desktop, "pidof noip2 || sudo /usr/local/bin/noip2")
    -- media servers
    , (Common,  "pidof mpd || mpd")
    -- , (Desktop, "python /opt/couchpotato/CouchPotato.py --daemon")
    -- , (Desktop, "python /opt/headphones/Headphones.py --daemon")
    , (Desktop, "python /opt/sickbeard/SickBeard.py --daemon")
    ]

-- | desktop apps
wmApps =
    [ -- desktop
      (Xmonad,  "conky -p 3 -q")
    , (Notion,  "conky -p 3 -q -c ~/.config/conky/conkyrc.notion")
    , (Common,  "fish -c video")
    -- , (Common,  "pidof compton || cb-compositor --start")
    , (Common,  "pidof nitrogen || nitrogen --restore")
    , (Common,  "pidof thunar || thunar --daemon")
    -- systray applets
    , (Notion,  "trayion")
    ]

systrayApps =
    [
      (Desktop, "artha")
    , (Common,  "autokey")
    , (Common,  "sleep 3s && clipit")
    -- , (Common,  "sleep 3s && gpe-clock")
    , (Common,  "synapse -s")
    , (Common,  "pidof dropbox || ~/.dropbox-dist/dropboxd")
    , (Netbook, "pidof nm-applet || nm-applet")
    , (Common,  "pidof xfce4-power-manager || sudo xfce4-power-manager")
    ]

xterm title resource = "xterm -T '" ++ title ++ "' -name '" ++ resource ++ "' -e"
term title role  = "xfce4-term -T '" ++ title ++ "' --role '" ++ role ++ "' -x"

-- | window apps
windowApps =
    -- some delays added to place master pane applications
    [ -- application windows
      (Common,  "pidof gvim || (sleep 2s && gvim)")
    , (Common,  "pidof luakit || (sleep 1s && luakit)")
    -- , (Desktop,  "lastfm")
    , (Common,  "pidof pidgin || pidgin")
    , (Desktop, "pidof pavucontrol || pavucontrol")
    , (Common,  "pidof rox || rox")
    -- , (Desktop, "pidof thunar || thunar")
    -- , (Desktop, "pidof tixati || tixati")
    -- , (Desktop, "pidof jumanji || (sleep 1s && jumanji http://thedarnedestthing)")
    , (Common,  "pidof uzbl-core || (sleep 1s && uzbl http://thedarnedestthing)")
    -- , (Desktop, term "BitTorrent" "bittorrent" ++ " rtorrent")
    , (Common,  term "IMAP" "imap" ++ " fish -c imap")
    , (Common,  term "IMAP Log" "imap" ++ " fish -c 'imap l'")
    , (Common,  "sleep 1s && " ++ term "Mail" "mail" ++ " fish -c sup")
    , (Common,  "sleep 2s && " ++ term "Music" "music" ++ " fish -c music")
    , (Desktop, term "NZB" "nzb" ++ " fish -c nzb")
    -- , (Desktop, term "Process" "process" ++ " htop")
    , (Desktop, term "Radio" "radio" ++ " fish -c radio")
    , (Common,  term "RSS" "rss" ++ " fish -c rss")
    -- , (Desktop, term "Shell" "text" ++ " fish")
    -- , (Desktop, term "Syslog" "syslog" ++ " fish -c 'tail syslog'")
    , (Common,  term "TODO" "todo" ++ " fish -c todo")
    , (Desktop, "sleep 1s && " ++ xterm "Twitter" "twitter" ++ " fish -c twitter")
    , (Desktop, term "Usenet" "usenet" ++ " fish -c usenet")
    ]

main = do
    host <- getHost
    wm   <- getWm
    -- map killall       conky:sessionApps
    map killall       sessionApps
    map killps        uniqueProcesses
    map (launch host) serverApps
    map (launch wm)   trayer host:wmApps
    map (launch host) systrayApps
    map (launch host) windowApps
  where
    -- conky = if doesFileExist home ++ "/.statusbar" && wm = Notion
    --             "conky"
    --         else
    --             "true"
    conky = "conky"

    trayer host = (Xmonad,  "trayer --monitor primary --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype pixel --width " ++ trayWidth host ++ " --transparent true --alpha 0 --tint 0x060606 --height 24 --distance 0")
      where
        trayWidth Desktop = 120
        trayWidth Netbook = 100
        trayWidth _       = 120

-- | For use in cross host configutions
data Env = Desktop | Netbook | Other | Notion | Xmonad | Common deriving Eq

-- | Determine the host
getHost = do
    host <- getSystemID
    case nodeName host of
         "luna"  -> return Desktop
         "monad" -> return Netbook
         _       -> return Other

getWm = do
    input <- readFile "~/.windowmanager"
    case input of
        "notion" -> return Notion
        "xmonad" -> return Xmonad
        _        -> return Common

