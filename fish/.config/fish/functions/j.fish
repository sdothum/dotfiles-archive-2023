function j --description 'replace autojump with fuzzy jump'
  switch (count $argv)
    case 0
      find -type d 2>/dev/null | grep -v '.deprecated' | fzf | read dir
    case 1
      if test -d $argv[1]
        find $argv[1] -type d 2>/dev/null | grep -v '.deprecated' | fzf | read dir
      else
        if pwd | grep -q $HOME
          find $HOME -type d 2>/dev/null | grep -v '.deprecated' | fzf -q $argv[1] | read dir
        else
          find -type d 2>/dev/null | grep -v '.deprecated' | fzf -q $argv[1] | read dir
        end
      end
    case 2
      if test -d $argv[1]
        find $argv[1] -type d 2>/dev/null | grep -v '.deprecated' | fzf -q $argv[2] | read dir
      else
        if pwd | grep -q $HOME
          find $HOME -type d 2>/dev/null | grep -v '.deprecated' | grep "$argv[1]" | fzf -q $argv[2] | read dir
        else
          find -type d 2>/dev/null | grep -v '.deprecated' | grep "$argv[1]" | fzf -q $argv[2] | read dir
        end
      end
  end
  cd $dir
end
