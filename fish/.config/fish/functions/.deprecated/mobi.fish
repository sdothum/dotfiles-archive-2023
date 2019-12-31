function mobi
  # shelf mobi book in author folder
  set author (echo $argv | sed -e 's/.mobi//' -e 's/ /\\ /g')
  [ -d $author ]; or mkdir -v $author
  echo /$author/
  eval "mv -v *"(echo $argv | sed 's/ /*/g')" '$author'"
end
