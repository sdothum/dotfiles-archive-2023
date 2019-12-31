function gnome-mplayer
    [ (count $argv) -gt 0 ]; and video gnome-mplayer (uri $argv); or video gnome-mplayer
end
