function diary-date
  touch -d(echo "$argv" | sed 's/.*\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\).wiki/\1/') $argv
end
