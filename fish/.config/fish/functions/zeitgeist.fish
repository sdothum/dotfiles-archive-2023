function zeitgeist
  trace .. rebuilding zeitgeist database
  zeitgeist-daemon --quit
  cd ~/.local/share/zeitgeist/
  command rm -rvf fts.index/
  zeitgeist-daemon &
  trace .. done
end
