function apt
  set packages $argv[(seq 2 (count $argv))]

  function history
    switch "$argv[1]"
      case install;         cat /var/log/dpkg.log | grep 'install '
      case upgrade remove;  cat /var/log/dpkg.log | grep $argv
      case rollback;        cat /var/log/dpkg.log | grep upgrade | \
                              grep "$2" -A10000000 | \
                              grep "$3" -B10000000 | \
                              awk '{print $4"="$5}'
      '*';                  cat /var/log/dpkg.log
    end
  end

  function !statoverride
    if [ -f /var/lib/dpkg/statoverride ]
      p aptitude; and attn ".. apt action in progress"; and false; and return
      sudo rm /var/lib/dpkg/statoverride
    end
    true
  end

  function sim
    attn ".. simulation only"
  end

  switch "$argv[1]"
    case [1];       !statoverride; and sudo aptitude
    case a;         [ -n "$packages" ]; or set packages (head -1 /etc/apt/apt.conf | sed 's/^.*"\(.*\)".*/\1/')
                    if [ -d ~/sync/dist/etc/apt/$packages ]
                      [ -f ~/sync/dist/etc/apt/$packages/preferences ]; or sudo rm /etc/apt/preferences
                      sudo cp -Rv ~/sync/dist/etc/apt/$packages/{apt.conf,preferences,sources.list,sources.list.d} /etc/apt/ 2>/dev/null
                    else
                      echo ".. apt a backports|media|stable|testing|unstable|sid"
                    end
    case C;         !statoverride; and sudo aptitude autoclean; and df -h
    case c;         sim; aptitude -s autoclean
    case D;         sudo apt-get upgrade; and sudo apt-get dist-upgrade; and eval ~/sync/dist/sid/netinstall-5post
    case d;         sim; apt-get -s dist-upgrade | egrep -v '^(Conf|Inst)'
    case F;         if-no "update apt-file database"; or begin
                      sudo apt-file update
                      command rm -f ~/.apt-file.update 2>/dev/null
                    end
    case f;         [ -f ~/.apt-file.update ]; and apt F; apt-file search $packages
    # see crunchbang-3install.fish for apt.error usage
    case h;         history $packages
    case i;         if !statoverride
                      sudo aptitude install $packages 2> ~/logs/apt.error
                      dcache &
                    end
    case I;         !statoverride; and sudo aptitude reinstall $packages 2> ~/logs/apt.error
    case l;         sudo less /var/log/apt/history.log
    case m;         sim; aptitude -s install $packages
    case p;         !statoverride; and sudo aptitude purge $packages 2> ~/logs/apt.error
    case Q;         dpkg-query -l
    case q;         aptitude show $packages
    case r;         !statoverride; and sudo aptitude remove $packages 2> ~/logs/apt.error
    case S;         apt-cache search $packages | sort | g "$packages"
    case s;         aptitude search $packages
    case t;         if !statoverride
                      sudo aptitude -t $argv[2] install $argv[(seq 3 (count $argv))] 2> ~/logs/apt.error
                      dcache &
                    end
    case U;         if !statoverride
                      sudo aptitude safe-upgrade
                      eval ~/sync/dist/sid/netinstall-5post
                      dcache &
                    end
    case u;         !statoverride; and sudo aptitude update; apt d
    # for unattended netinstall-4apps
    case Y;         !statoverride; and sudo aptitude -y --allow-untrusted install $packages 2> ~/logs/apt.error
    case y;         !statoverride; and sudo aptitude install $packages 2> ~/logs/apt.error
    case '*';       echo ".. apt  'curses  a'pply  -s autoc/C'lean  -s d/D'ist-upgrade  apt-F'ile  f'ind  h'istory  reI'nstall  i'nstall  sim'ulate  l'og  p'urge  q/Q'uery  r'emove  s/S'earch  t'arget-install  safe-U'pgrade  u'pdate"
  end
end
