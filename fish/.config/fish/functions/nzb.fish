function nzb
  set list_delay 60s
  set queue $argv[(seq 2 (count $argv))]
  switch "$argv[1]"
    case [1];     if !p "nzbget.*.nzbget.conf"
                    # and nzbget -c ~/.nzbget.conf -o OutputMode=(!p notion; and echo colored; or echo curses) -s
                    nzbget -c ~/.nzbget.conf -o OutputMode=curses -s
                  else
                    underline (date '+%A -%-I:%M.%S%P-')
                    nzbget -c ~/.nzbget.conf -L G
                  end
    case b;       nzbget -c ~/.nzbget.conf -E B $queue; nzb
    case d;       nzbget -c ~/.nzbget.conf -E D $queue; nzb
    case D;       nzb
                  if-yes "clear entire download queue"; and nzbget -c ~/.nzbget.conf -E D 1-999999
    case l;       echo ".. tail -f /tmp/nzbget.log | g 'error|warning'"
                  tail -f /tmp/nzbget.log | g 'error|warning'
    case L;       while true; nzb; sleep $list_delay; end
    case p;       nzbget -c ~/.nzbget.conf -E P $queue; nzb
                  # if statement stops restarted download on ctrl-c break (when P option is toggled successively)
    # case P;       if [ -f ~/.nzbget.paused ]; nzb U; else; nzbget -c ~/.nzbget.conf -P; touch ~/.nzbget.paused; nzb; end
    case P;       if [ -f ~/.nzbget.paused ]
                    nzb U
                  else
                    nzbget -c ~/.nzbget.conf -P
                    touch ~/.nzbget.paused
                    nzb
                  end
    case q;       nzbget -c ~/.nzbget.conf -Q
    case r;       nzbget -c ~/.nzbget.conf --rate $queue
    case s;       nzb | egrep -i $queue
    case t;       nzbget -c ~/.nzbget.conf -E T $queue; nzb
    case u;       nzbget -c ~/.nzbget.conf -E U $queue; nzb
    case U;       nzbget -c ~/.nzbget.conf -U
                  command rm -f ~/.nzbget.paused 2>/dev/null
                  !p "sleep $list_delay"; and nzb L; or nzb
    case h '*';   echo ".. nzbget  'start/show  b'ottom  d'elete|D(all)  h'elp  l'og  L'ist  p'ause|P(toggle)  q'uit  r'ate  s'earch  t'op  u'npause|U "
  end
end
