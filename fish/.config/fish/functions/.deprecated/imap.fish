function imap
  switch "$argv[1]"
    case [1];   if !p "python.*offlineimap.*offlineimaprc"
                  offlineimap -c ~/.offlineimaprc -u basic > /tmp/offlineimap.log 2>&1 &
                  echo ".. offlineimap running"
                end
                if-yes ".. watch offlineimap log"; and imap l
    case 1;     offlineimap -c ~/.offlineimaprc -o -u basic > /tmp/offlineimap.log 2>&1 &; imap l
    case C;     rm -f ~/.imapfilter/certificates; imap f
    case F;     if [ -f ~/.offlineimapfilter ]
                  command rm -rf ~/.offlineimapfilter
                  echo ".. imapfilter processing off"
                else
                  touch ~/.offlineimapfilter
                  echo ".. imapfilter processing on"
                end
    case f;     if !p /usr/bin/offlineimap; and !p /usr/bin/imapfilter
                  [ (count $argv) -eq 1 ]
                    and imapfilter -v -c ~/.imapfilter/config.lua
                    or  imapfilter -v -c ~/.imapfilter/$argv[2].lua; end
    case I;     killall offlineimap 2>/dev/null
                if-no "delete all maildirs and reload from gmail"; or begin;
                  command rm -rf ~/.offlineimap/*
                  command rm -rf ~/.mail/*
                  command rm -rf ~/.mutt/cache/*
                  imap 1
                end
    case L;     while [ (p "imapfilter.*imapfilter/config.lua" | wc -l) -lt 2 ]; sleep 2s; end
                echo ".. tail -f /tmp/imapfilter.log"; tail -f /tmp/imapfilter.log
    case l;     echo ".. tail -f /tmp/offlineimap.log"; tail -f /tmp/offlineimap.log
    case Q;     !p "python.*offlineimap.*offlineimaprc"; and offlineimap -c ~/.offlineimaprc -u quiet > /tmp/imapfilter.log 2>&1 &
    case q;     killall imapfilter 2>/dev/null; kill -9 (p "python.*offlineimap.*offlineimaprc"|awk '{print $2;}') 2>/dev/null
    case r;     imap q; imap
    case '*';   echo ".. imap  'start  1'pass  renewC'ertificate  imapF|f'ilter{ config}  reI'nitialize  L|l'og  Q'uiet  q'uit  r'estart"
                echo "   renewCertificate to correct periodic fingerprint mismatch"
  end
end
