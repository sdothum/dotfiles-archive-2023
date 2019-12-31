function keywords
  # "keywords file grouplist"
  # "keywords string grouplist"
  trace 3 [inlist] egrep -i \"\((awk '{ printf ("%s|", $0) ; }' "$argv[2]" | sed 's/|$//')\)\" \"$argv[1]\"
  [ -f "$argv[1]" ]
    and eval egrep -i \"\((awk '{ printf ("%s|", $0) ; }' "$argv[2]" | sed 's/|$//')\)\" \"$argv[1]\"
    or  echo "$argv[1]" | eval egrep -i \"\((awk '{ printf ("%s|", $0) ; }' "$argv[2]" | sed 's/|$//')\)\"
end
