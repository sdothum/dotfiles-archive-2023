function thunar-reset
  set save (random)
  killall thunar
  echo ".. saving ~/.local/share/Trash.$save"
  command mv -f ~/.local/share/Trash ~/.local/share/Trash.$save
  command rm -rf ~/.cache/sessions 2>/dev/null
  command rm -rf ~/.cache/Thunar 2>/dev/null
  thunar --daemon &
end
