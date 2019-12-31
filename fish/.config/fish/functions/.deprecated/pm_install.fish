function pm_install
  heading+ $argv
  if not pm_query $argv
    pre_install $argv
    sudo pacman -S --noconfirm $argv
  end
  # do post install for previously installed package dependency
  post_install $argv
end
