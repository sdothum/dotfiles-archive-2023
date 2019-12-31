function burn
  switch "$argv[1]"
    case [1];   echo ".. burn  dvd | iso | {data}"
    case dvd;   [ (ll /net/downloads/dvd/ | wc -l) -gt 1 ]; and ll /net/downloads/dvd/
                  and if-no "erase directory"; and return
                dvdbackup -i /dev/dvd -o /net/downloads/dvd/ -M
                set title (basename /net/downloads/dvd/*)
                genisoimage -dvd-video -udf -V "$title" -o /net/downloads/dvd/dvd.iso "$title"
                if-yes "burn iso to dvd"; and burn iso
    case iso;   [ -f /net/downloads/dvd/dvd.iso ]; and growisofs -dvd-compat -Z /dev/dvd=/net/downloads/dvd/dvd.iso
    case '*';   if [ (ll /media/cdrom0/ | wc -l) -eq 1 ]
                  if-yes "new session (initialize)"; and growisofs -Z /dev/dvd -R -J $argv
                else
                  if-yes "merge session (append)"; and growisofs -M /dev/dvd -R -J $argv
                end
                echo ".. eject and select cdrom in thunar before next merge"
    end
end
