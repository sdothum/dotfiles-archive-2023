function sshx
  [ (hostname) = monad ]; and set host luna; or set host monad
  ssh -Y $USER@$host "$argv"
end
