function vlc
    [ (count $argv) -gt 0 ]; and video vlc (uri $argv); or video vlc
end
