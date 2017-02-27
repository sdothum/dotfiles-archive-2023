function j --description 'fuzzy jump'
  set filter '.deprecated|.hg'
  switch (count $argv)
    case 0
      find -type d 2>/dev/null | egrep -v $filter | fzf | read dir
    case 1
      if test -d $argv[1]
        find $argv[1] -type d 2>/dev/null | egrep -v $filter | fzf | read dir
      else
        find $HOME -type d 2>/dev/null | egrep -v $filter | fzf -q $argv[1] | read dir
      end
    case 2
      if test -d $argv[1]
        find $argv[1] -type d 2>/dev/null | egrep -v $filter | fzf -q $argv[2] | read dir
      else
        find $HOME -type d 2>/dev/null | egrep -v $filter | egrep "$argv[1]" | fzf -q $argv[2] | read dir
      end
  end
  cd $dir
end
