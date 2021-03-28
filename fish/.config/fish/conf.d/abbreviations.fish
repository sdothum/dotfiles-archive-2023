# sdothum - 2016 (c) wtfpl

# Fish Shell
# ══════════════════════════════════════════════════════════════════════════════

# ...................................................................... Package

if arch
  abbr pkgs 'comm -23 <(pacman -Qeq | sort) <(pacman -Qgq base base-devel | sort)'
  abbr pn 'env D=N pd'
  abbr pq 'pacman -Qii'
end

# ...................................................................... Process

abbr K 'env sig=-KILL k'
abbr KK 'env sig=-KILL kk'
abbr sv 'service'
if void
  abbr svd  'service disable'
  abbr svdn 'service down'
  abbr sve  'service enable'
  abbr svm  'service mask'  # at boot
  abbr svr  'service restart'
  abbr svs  'service status'
  abbr svu  'service up'
  abbr svv  'service UP'
else
  abbr svd  'service disable'
  abbr sve  'service enable'
  abbr svi  'service info'
  abbr svl  'service reload'
  abbr svm  'service mask'
  abbr svr  'service restart'
  abbr svs  'service start'
  abbr svt  'service stop'  # terminate
  abbr svu  'service unmask'
end

# ....................................................................... Device

abbr close 'eject -t'
abbr left_shift_key 'test -f /etc/X11/Xmodmap ;and xmodmap /etc/X11/Xmodmap'
abbr mount 'sudo mount'
abbr umount 'sudo umount'

# ....................................................................... System

abbr blame 'systemd-analyze blame'
abbr font-manager 'font-manager ; sudo rm -f ~/.fonts.conf'
abbr fontmatrix 'fontmatrix ; sudo rm -f ~/.fonts.conf'
abbr gtop 'glances'
abbr htop 'htop --sort-key PERCENT_CPU'
abbr ttop 'htop --tree'
abbr iotop 'sudo iotop'
abbr lpr 'lpr -P hp-LaserJet-1320-series'
abbr screenfetch 'fetch'
abbr services "systemctl list-units -t service --no-pager --no-legend | grep active | egrep -v 'systemd|exited' | cut -d' ' -f1"
abbr bash 'bash -norc'
abbr sh 'rlwrap sh'
abbr time '/usr/bin/time -p'
abbr traceroute 'mtr --report -c 1'
abbr who 'command w'

# ...................................................................... Network

abbr friendlybear 'smbclient -R bcast //friendlybear/patricia motu om'

# ......................................................................... File

abbr cp 'cp -i'
abbr cpl 'cp -iLRfv'
abbr cpv 'cp -iv'
abbr gprename 'gprename $PWD'
abbr m 'less'
abbr mv 'mv -i'
abbr mvv 'mv -iv'
abbr n 'env VISUAL=vim nnn'
abbr r 'vifm'
abbr R 'env VISUAL=vim ranger'
abbr rm 'rm -i'
abbr rmv 'rm -iv'
# abbr t 'tail -f'

# .................................................................... Directory

abbr dus "/usr/bin/du -hs * | sort -h"
abbr l 'ls -A'
abbr l1 'ls -1'
abbr ldot 'ls -lAd .*'
abbr ll 'ls -lA'
abbr llr 'ls -lAR'
abbr lr 'ls -LAR'
abbr lt 'll -t'
abbr path 'echo $PATH'
abbr pp 'pwd'
abbr tree 'sudo tree -aCF'
abbr treed 'sudo tree -aCdF'

# ....................................................................... Search

abbr fd 'find -type d'
abbr ff 'find -type f'
abbr f 'find -iname'
# abbr g 'ack --ignore-case'
# abbr gl 'ack -l --ignore-case'
abbr locate 'sudo locate'
abbr mgrep 'pcregrep -r -M'
abbr w 'which'

# ...................................................................... Desktop

abbr c 'clear ; setterm -cursor on'
abbr cursor 'setterm -cursor on'
abbr gaps 'rlwrap gaps'
abbr h: 'ls -l /tmp/herbstluftwm:*'
abbr hc 'herbstclient'
abbr herbstluftwm ". $HOME/.config/herbstluftwm/config/ENV"
abbr X x

# ......................................................................... Edit

abbr ddf 'dirdiff'
abbr de 'dmenu - edit'
abbr dp 'dmenu - projects'
abbr dr 'dmenu - run'
abbr ds 'dmenu - scripts'
abbr nv 'nvpy'
abbr vd 'gvim -d --role=gvimdiff'
abbr sd 'sdiff -b -E -W -w(tput cols)'
abbr vdarchive 'dirdiff ./ /net/archive(pwd)'
abbr vdbackup 'dirdiff ./ /net/backup(pwd)'

# .................................................................. Development
 
# abbr ghc 'ghc -dynamic'  # arch repo only
abbr ghc 'stack ghc'
abbr ghcc 'stack build'
abbr ghcx 'stack runghc'
abbr ghci 'stack exec ghci'
abbr git1 'git clone --depth 1'
abbr mysql 'mysql -h localhost -u root -p'
abbr perli 'perl -de 1'

# .................................................................. Application

abbr bc 'bcd'
abbr calc 'speedcrunch'
abbr d 'dict'
abbr kindle 'dmenu kindle azw3'
abbr dotfiles 'rlwrap dotfiles'
abbr handbrake 'ghb'
abbr music '!p ncmpcpp ;and ncmpcpp'
abbr reqk 'qk stop; qk start'
abbr scrot 'scrot -e "mv \$f /net/photos/batchqueue/"'
abbr ss 'sc-im'
abbr td 'td -i'
# abbr todo 'rlwrap todo-screen'
abbr uterm 'urxvt -sh 1'
abbr wl 'wc -l'
