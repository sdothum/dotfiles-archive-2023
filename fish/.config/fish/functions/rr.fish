function rr
  set filename ~/.rubyroom/untitled.(date '+%s')
  if [ (count $argv) -gt 0 ]
    set filename "$argv"
  end
  if [ ! -f "$filename" ]
    mkdir ~/.rubyroom 2>/dev/null
    touch "$filename"
  end

  rvm clear; rubyroom "$filename"

  if [ -s "$filename" ]
    set folded (mktemp)
    sed 's/   */ /g' "$filename" | fold -s -w 72 > $folded
    mv -f $folded "$filename"
  else
    command rm -f "$filename"
  end
end
