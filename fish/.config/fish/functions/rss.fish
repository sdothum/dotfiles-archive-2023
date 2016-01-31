function rss
  set BROWSER /usr/bin/local/luakit

  function news
    # xrdb -merge ~/.Xresources
    ping -c1 luna 2>&1 >/dev/null; and sed -i 's,http://localhost/tt-rss,http://luna/tt-rss,' ~/.config/newsbeuter/config; \
      or sed -i 's,http://luna/tt-rss,http://localhost/tt-rss,' ~/.config/newsbeuter/config;
    # xterm -T 'RSS' -name 'RSS' -e $argv &
  end

  switch "$argv[1]"
    # case [1];   !p newsbeuter; and news newsbeuter -C ~/.config/newsbeuter/config; or p newsbeuter
    case [1];   !p newsbeuter; and news; and newsbeuter -C ~/.config/newsbeuter/config; or p newsbeuter
    case I;     command rm -f ~/.newsbeuter/cache.db 2>/dev/null; rss
    case i;     !p newsbeuter; and news newsbeuter -C ~/.config/newsbeuter/config -i $argv[2]
    case '*';   echo ".. news  'start  I'nitialize i'mport{ opml}"
  end
end
