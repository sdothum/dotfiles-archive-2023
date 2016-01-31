function gem_install
  # gem list matches partial strings
  if gem list $argv | grep "^$argv "
    heading+ "$argv   [ INSTALLED ]"
  else
    heading+ $argv
    gem install $argv --no-rdoc --no-ri
  end
end
