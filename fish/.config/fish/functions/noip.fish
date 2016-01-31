function noip
  switch "$argv[1]"
    case [1];   sudo /usr/local/bin/noip2; noip d
    case C;     sudo /usr/local/bin/noip2 -C
    case c;     ssh thedarnedestthing.no-ip.org
    case D;     sudo /usr/local/bin/noip2 -D pid
    case d;     sudo /usr/local/bin/noip2 -S
    case k;     sudo /usr/local/bin/noip2 -K pid
    case "*";   echo ".. noip  'start  C'onfigure  c'onnect  D'ebug  d'isplay  k'ill"
  end
end
