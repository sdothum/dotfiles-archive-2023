function kf
  function pfish
    p fish|awk '{ if ($4 > 0) print $0 }'
  end

  if [ (pfish | /usr/bin/wc -l) -gt 0 ]
    t | grep fish
    set pids (pfish | awk '{print $2}')
    if-yes "kill /fish/ $pids"; and sudo kill -9 $pids
  end
end
