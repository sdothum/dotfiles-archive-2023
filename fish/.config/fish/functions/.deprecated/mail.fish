function mail
  rvm 1.9
  set -x HELIOTROPE_LOG_LEVEL debug
  set -x TURNSOLE_LOG_LEVEL debug
  set pwd $PWD
  cd /opt/heliotrope
  switch "$argv[1]"
    case [1];   !p heliotrope-server; and ruby -I lib bin/heliotrope-server -d ~/.heliotrope --with-imap &
                !p turnsole; and cd /opt/turnsole; and ruby -I lib -I /opt/heliotrope/lib bin/turnsole --verbose
    case a;     !p heliotrope-server; or ruby -I lib bin/heliotrope-add -a ~/.mail/$argv[2] --verbose
    case I;     !p heliotrope-server; and ruby -I lib bin/heliotrope-import -a ~/.mail/$argv[2] -d ~/.heliotrope --verbose
    case l;     echo "tail -f /tmp/heliotrope.log"; tail -f /tmp/heliotrope.log
    case q;     killall heliotrope-server 2>/dev/null
    case '*';   echo ".. mail  'start  a'dd{ maildir}  I'mport{ maildir }  l'og  q'uit"
  end
  cd $pwd
end

