function screen
  switch "$argv[1]"
    case [1];   if !p /usr/bin/screen
                  command screen
                else
                  [ (p /usr/bin/screen | wc -l) -lt 2 ]; and /usr/bin/screen -d -r
                end
    case q;     killall screen
                killall irssi
                killall mutt
                killall ncmpcpp
                killall newsbeuter
                killall nzbget
                killall offlineimap
                killall pyradio
                killall ranger
                killall rtorrent
                killall slrn
                killall todo
    case '*';   echo ".. screen  'start  q'uit"
  end
end
