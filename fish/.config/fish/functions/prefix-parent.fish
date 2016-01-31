function prefix_parent
  set prefix (basename $PWD)
  for i in *
    [ -d $i ]; and echo "$prefix $i"
    [ -d $i ]; and command mv $i "../$prefix $i"
  end
end
