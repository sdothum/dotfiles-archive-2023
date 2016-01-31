function sup
  set SHELL /bin/sh
  [ -f /usr/bin/sup ]; and set -g bin /usr/bin; or set -g bin /var/lib/gems/1.9.1/gems/sup-0.15.0/bin
  # set -g bin (find /var -path '*gems*/sup-*/bin')
  set -x SUP_LOG_LEVEL debug
  # reduce ruby garbage collection activity (to speed up sup!)
  set -x RUBY_GC_MALLOC_LIMIT 256000000
  set -x RUBY_HEAP_MIN_SLOTS 600000
  set -x RUBY_HEAP_SLOTS_INCREMENT 200000
  set -x RUBY_HEAP_FREE_MIN 100000
  set BROWSER luakit
  # force color, otherwise colors inverted (for wombat scheme)
  set TERM xterm-256color

  function killsup
    kill -9 (pid $bin/sup) 2>/dev/null
    command rm -f ~/.sup/lock; command rm -f ~/.sup/xapian/flintlock 2>/dev/null
  end

  switch "$argv[1]"
    case [1];   if [ (p $bin/sup | wc -l) -eq 0 ]
                  command rm -f ~/.sup/lock
                  if [ -d ~/.sup/xapian ]
                    command cp -fv ~/.sup/sources.yaml.conf ~/.sup/sources.yaml
                  else
                    command cp -fv ~/.sup/sources.yaml.init ~/.sup/sources.yaml
                    touch ~/.sup-archive
                  end
                  # mb2md.pl -s ~/.sup/sent.mbox -d ~/.mail/sdothum/INBOX >> /tmp/sup.log 2>&1
                  # archivemail -d0 -s.%y%m%d.%H%M%S ~/.sup/sent.mbox >> /tmp/sup.log 2>&1
                  eval $bin/sup 2>> /tmp/sup.log; command rm -f ~/.sup-archive
                else
                  echo ".. instance locked: force restart with =>> sup f"
                end
    case d;     !p $bin/sup; and eval $bin/sup-dump > ~/.sup/dump
    case f;     if-yes "force sup"; and begin
                  killsup; command rm -f ~/.sup/lock
                end; sup
    case I;     if-yes "initialize xapian database"; and begin
                  killsup; command rm -Rf ~/.sup/xapian 2>/dev/null
                end; sup
    case l;     echo ".. tail -f /tmp/sup.log"; tail -f /tmp/sup.log
    case M;     if-yes "sync maildirs"; and begin
                  killsup; eval ~/bin/sup-sync-ok-hack
                end; sup
    case r;     if-yes "sup restore"; and begin
                  killsup; eval $bin/sup-sync --restored --restore ~/.sup/dum
                end; sup
    case S;     if-yes "sup restore"; and begin
                  killsup; eval $bin/sup-sync 2>> /tmp/sup.log
                end; sup
    case s;     if-yes "sup restore"; and begin
                  killsup; eval $bin/sup-sync -v maildir:~/.mail/sdothum/user/$argv[2]
                end; sup
    case '*';   echo ".. sup  'start  d'ump  f'orce  I'nitialize  l'og  sync-M'aildirs  r'estore  reS'ync  s'ync{ folder}"
  end
end
