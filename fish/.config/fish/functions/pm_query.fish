function pm_query
  set query (pacman -Qs $argv | grep "local/$argv .*")
  [ -n "$query" ]; and heading ' ' (echo $query | sed -e 's|local/||' -e 'a\ \ [ INSTALLED ]'); or return 1
end
