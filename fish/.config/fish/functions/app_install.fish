function app_install
  # using a non-arch installer application
  heading+ $argv
  if which $argv 2>&1 >/dev/null
    pre_install $argv
    eval $argv
    post_install $argv
  end
end
