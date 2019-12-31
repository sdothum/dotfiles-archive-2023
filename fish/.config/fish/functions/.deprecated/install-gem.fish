function install-gem
  # drawline ':'
  echo
  trace (set_color -o red)"─── installing gem ───" (set_color -o cyan)"$argv"
  echo
  gem install $argv --no-rdoc --no-ri
end
