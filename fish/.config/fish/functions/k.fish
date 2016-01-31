function k
  if [ (count $argv) -gt 0 ]
    if [ (p $argv | /usr/bin/wc -l) -gt 0 ]
      p $argv
      if-no "kill /$argv/ "(echo (pid $argv)); or sudo kill -9 (pid $argv)
    end
  else
    # dispose of any runaway fish sessions
    k (session-apps)
  end
end
