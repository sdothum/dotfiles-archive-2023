function cf --description 'fuzzy directory'
  switch (count $argv)
    case 0
      find -type d 2>/dev/null | fzf | read dir
    case 1
      find $argv[1] -type d 2>/dev/null | fzf | read dir
    case 2
      find $argv[1] -type d 2>/dev/null | fzf -q $argv[2] | read dir
  end
  cd $dir
end
