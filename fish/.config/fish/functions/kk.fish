function kk
  if [ (count $argv) -gt 0 ]
    if [ (p $argv | /usr/bin/wc -l) -gt 0 ]
      p $argv
      if-yes "kill /$argv/ "(echo (pid $argv)); and sudo kill -9 (pid $argv)
    end
  else
    # special condition for clearing notion apps prior to cycling window manager
    # dispose of any runaway fish sessions
    kk (session-apps)
  end
end
