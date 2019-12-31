function supgem
  # set lib -I/opt/sup/lib -I/usr/lib/ruby/1.9.1/x86_64-linux -I/usr/lib/ruby/1.9.1
  set -g bin /usr/local/bin
  set -x SUP_LOG_LEVEL debug
  set BROWSER /usr/local/bin/luakit

  function killsup
    kill -9 (pid $bin/sup) 2>/dev/null
    command rm -f ~/.sup/lock; command rm -f ~/.sup/xapian/flintlock 2>/dev/null
  end

  switch "$argv[1]"
    case [1];   if [ (p $bin/sup | wc -l) -eq 0 ]
                  command rm -f ~/.sup/lock
                  [ ! -d ~/.sup/xapian ]; and command rm -f ~/.sup/labels.txt; and touch ~/.sup-archive
                  # ruby $lib $bin/sup 2> /tmp/sup.log; command rm -f ~/.sup-archive
                  command sup 2> /tmp/sup.log; command rm -f ~/.sup-archive
                else
                  echo ".. instance locked: force restart with =>> sup f"
                end
    case d;     !p $bin/sup; and $bin/sup-dump > ~/.sup/dump
    case f;     if-yes "force sup"; and begin; killsup; end; sup
    case I;     if-no "initialize xapian database"; or begin; killsup; command rm -Rf ~/.sup/xapian 2>/dev/null; end; sup
    case l;     echo ".. tail -f /tmp/sup.log"; tail -f /tmp/sup.log
    case M;     if-yes "sync maildirs"; and begin; killsup; ruby $lib ~/bin/sup-sync-ok-hack; end; sup
    case r;     if-yes "sup restore"; and begin; killsup; $bin/sup-sync --restored --restore ~/.sup/dump; end; sup
    case S;     if-yes "sup restore"; and begin; killsup; ruby $lib $bin/sup-sync 2>> /tmp/sup.log; end; sup
    case s;     if-yes "sup restore"; and begin; killsup; ruby $lib $bin/sup-sync -v maildir:~/.mail/sdothum/user/$argv[2]; end; sup
    case '*';   echo ".. sup  'start  d'ump  f'orce  I'nitialize  l'og  sync-M'aildirs  r'estore  reS'ync  s'ync{ folder}"
  end
end
