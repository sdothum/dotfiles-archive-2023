function j --description 'fuzzy jump'
  if test (count $argv) -gt 0
    if test $argv[1] = '-'
      set refresh $argv[1]
      set --erase argv[1]
    end
  end
  switch (count $argv)
    # use dynamic cache to speed up subsequent matches (especially arm)
    case 0
      if pwd | grep -q $HOME
        fcache $refresh $HOME | fzf | read dir
      else
        fcache $refresh $PWD | fzf | read dir
      end
    case 1
      if test -d $argv[1]
        fcache $refresh $argv[1] | fzf | read dir
      else
        fcache $refresh $HOME | fzf -q $argv[1] | read dir
      end
    case 2
      if test -d $argv[1]
        fcache $refresh $argv[1] | fzf -q $argv[2] | read dir
      else
        fcache $refresh $HOME | egrep "$argv[1]" | fzf -q $argv[2] | read dir
      end
  end
  test -n $dir ;and cd $dir
end
