function walk
  trace $PWD
  for item in *
    if [ -d "$item" ]
      set pwd $PWD; cd $item
      walk $argv
      cd $pwd
    else
      eval $argv (escape "$item")
    end
  end
end
