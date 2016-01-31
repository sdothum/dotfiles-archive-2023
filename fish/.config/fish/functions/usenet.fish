function usenet
  set host (grep "^nnrpaccess" ~/.slrnrc | awk '{print $2;}' | sed 's/"//g')
  switch "$argv[1]"
    case [1];   !p slrn; and command slrn -h $host
    case f;     killall slrn 2>/dev/null; usenet
    case I;     command slrn -h $host --create
                command slrn -h $host -d
                usenet
    case '*';   echo ".. usenet  'start  f'orce  reI'nitialize"
  end
end
