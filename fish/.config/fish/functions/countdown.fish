function countdown
  switch "$argv[1]"
    case [1];   echo ".. countdown [minutes]"
    case '*';   date -d "now + $argv minutes" > ~/.countdown; echo "::" (cat ~/.countdown)
  end
end
