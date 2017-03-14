function j --description 'fuzzy jump'
  switch (count $argv)
    # use dynamic cache to speed up subsequent matches (especially arm)
    case 0
      if pwd | grep -q $HOME
        fcache d $HOME | fzf | read dir
      else
        fcache d $PWD | fzf | read dir
      end
    case 1
      if test -d $argv[1]
        fcache d $argv[1] | fzf | read dir
      else
        fcache d $HOME | fzf -q $argv[1] | read dir
      end
    case 2
      if test -d $argv[1]
        fcache d $argv[1] | fzf -q $argv[2] | read dir
      else
        fcache d $HOME | egrep "$argv[1]" | fzf -q $argv[2] | read dir
      end
  end
  test -z $dir ;or cd $dir
end
