for i in *
  for j in (egrep '^( |\+|\?|g) ' $i | sed -r 's/..([^ ]*).*/\1/')
    grep $j ../nix-packages
      and echo $j >>$i.found
      or echo $j >>$i.notfound
  end
end
