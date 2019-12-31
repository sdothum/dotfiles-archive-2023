function ffocuswriter
  set folded (mktemp)
  focuswriter $argv
  fold -s -w 72 $argv > $folded
  mv $folded $argv
end
