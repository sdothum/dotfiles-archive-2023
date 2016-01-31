function aur_install
  function install
    pre_install $argv[(count $argv)]
    eval $argv
  end

  heading+ $argv[1]
  if [ (count $argv) -eq 1 ]
    pm_query $argv; or install yaourt -S --noconfirm $argv
  else if not pm_query $argv[1]
    annotate "$argv[2]"
    install yaourt -S $argv[1]
  end
  # do post install for previously installed package dependency
  post_install $argv[1]
end
