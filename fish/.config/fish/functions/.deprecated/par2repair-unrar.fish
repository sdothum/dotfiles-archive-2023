function par2repair-unrar
  cd (dirname $argv)
  ll | grep -q '\.par2$'; and par2repair "$argv" *
  if [ -f *.r01 ]
    set basename (basename *.r01 .r01)
    if [ -f $basename.rar ]
      unrar $basename.rar
    else if [ -f $basename.r00 ]
      unrar $basename.r00
    else if [ -f $basename.r01 ]
      unrar $basename.r01
    end
  else if [ -f *.part1.rar ]
    set basename (basename *.part1.rar .part1.rar)
    if [ -f $basename.part0.rar ]
      unrar $basename.part0.rar
    else if [ -f $basename.part1.rar ]
      unrar $basename.part1.rar
    end
  else if [ -f *.part01.rar ]
    set basename (basename *.part01.rar .part01.rar)
    if [ -f $basename.part00.rar ]
      unrar $basename.part00.rar
    else if [ -f $basename.part01.rar ]
      unrar $basename.part01.rar
    end
  else if [ -f *.part001.rar ]
    set basename (basename *.part001.rar .part001.rar)
    if [ -f $basename.part00.rar ]
      unrar $basename.part00.rar
    else if [ -f $basename.part001.rar ]
      unrar $basename.part001.rar
    end
  else if [ -f *.001 ]
    unrar *.001
  else
    echo -n "..rar not extracted"
    read -p press-enter
  end
  cd -
end
