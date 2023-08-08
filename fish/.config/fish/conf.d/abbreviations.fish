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
abbr sv 'sv'  # override service abbrev (from fish-completions)
if alpine
  abbr sva  'sv add'
  abbr svd  'sv delete'
  abbr svi  'sv status'
  abbr svl  'sv list'
  abbr svr  'sv restart'
  abbr svs  'sv start'
  abbr svx  'sv stop'
else if void
  abbr svd  'sv disable'
  abbr svdn 'sv down'
  abbr sve  'sv enable'
  abbr svm  'sv mask'  # at boot
  abbr svr  'sv restart'
  abbr svs  'sv status'
  abbr svu  'sv up'
  abbr svup 'sv UP'
else
  abbr svd  'sv disable'
  abbr sve  'sv enable'
  abbr svi  'sv info'
  abbr svl  'sv reload'
  abbr svm  'sv mask'
  abbr svr  'sv restart'
  abbr svs  'sv start'
  abbr svx  'sv stop'  # terminate
  abbr svu  'sv unmask'
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
abbr services "systemctl list-units -t service --no-pager --no-legend | grep active | grep -E -v 'systemd|exited' | cut -d' ' -f1"
abbr bash 'bash -norc'
abbr sh 'rlwrap dash'
abbr time '/usr/bin/time -p'
abbr traceroute 'mtr --report -c 1'
abbr who 'command w'

# ...................................................................... Network

abbr friendlybear 'smbclient -R bcast //friendlybear/patricia motu om'

# ......................................................................... File

abbr c 'doas cat'
abbr cp 'cp -i'
abbr cpf 'cp -rf'
abbr cpl 'cp -iLRfv'
abbr cpr 'cp -rf'
abbr cpv 'cp -iv'
abbr gprename 'gprename $PWD'
abbr m 'less'
abbr mv 'mv -i'
abbr mvf 'mv -f'
abbr mvv 'mv -iv'
abbr rm 'rm -i'
abbr rmf 'rm -rf'
abbr rmr 'rm -rf'
abbr rmv 'rm -iv'
abbr s 'sort -n'
abbr t 'tail -f'

# .................................................................... Directory

set -l HUMAN h

abbr dus "/usr/bin/du -hs * | sort -h"
abbr l 'ls -A'
abbr l1 'ls -1'
abbr ldot "ls -lAd$HUMAN .*"
abbr ll "ls -lA$HUMAN"
abbr llr "ls -lAR$HUMAN"
abbr lr "ls -LAR"
abbr ls "ll -S$HUMAN"
abbr lt "ll -t$HUMAN"
abbr path 'echo $PATH'
abbr pp 'pwd'
abbr tree 'sudo tree -aCF'
abbr treed 'sudo tree -aCdF'

# ................................................................. File manager

abbr N 'nnn -p -'  # cmd .. (N) file picker mode
abbr n 'nnn'
abbr nb 'nnn -s ebooks'
abbr r 'vifm'
abbr R 'ranger'

# ....................................................................... Search

abbr fd 'sudo find -type d'
abbr ff 'sudo find -type f'
abbr f 'sudo find -iname'
abbr g 'ugrep --ignore-case'
abbr locate 'sudo locate'
abbr mgrep 'pcregrep -r -M'
abbr w 'which'
function ww; cd (dirname (which $argv 2>/dev/null) 2>/dev/null); test $HOME = (pwd) && begin ditto "$argv" 'not found'; cd -; end; end  # no 755 in $HOME

# ...................................................................... Desktop

abbr cl 'clear ; setterm -cursor on'
abbr cursor 'setterm -cursor on'
abbr gaps 'rlwrap gaps'
abbr h: 'ls -l /tmp/herbstluftwm:*'
abbr hc 'herbstclient'
abbr herbstluftwm ". $HOME/.config/herbstluftwm/config/ENV"
abbr X x

# ......................................................................... Edit

abbr ddf 'dirdiff'
abbr ddfn 'dirdiff (nnn -p -)'              # file picker mode
abbr de 'dmenu - edit'
abbr dp 'dmenu - projects'
abbr dr 'dmenu - run'
abbr ds 'dmenu - scripts'
abbr nv 'nvpy'
abbr vd 'v -d --role=gvimdiff'
abbr vdn 'v -d --role=gvimdiff (nnn -p -)'  # file picker mode
abbr sd 'sdiff -b -E -W -w(tput cols)'
abbr vdarchive 'dirdiff ./ /net/archive(pwd)'
abbr vdbackup 'dirdiff ./ /net/backup(pwd)'
abbr vsh "v $HOME/.local/share/fish/fish_history"

# ........................................................................ Regex

abbr list "ls -A1 | tr '\n' ' ' | sed 's/ /|/g; s/|\$//'"

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

abbr \? 'chatgpt'
abbr \?\? 'chatgpt -n'  # new conversation
abbr \@ 'aichat'
abbr \@s 'aichat -r shell'
abbr \@c 'aichat -r c'
abbr \@g 'aichat -r go'
abbr \@j 'aichat -r julia'
abbr \@p 'aichat -r python'
abbr \@r 'aichat -r ruby'
abbr \@R 'aichat -r rust'
abbr bc 'bcd'
abbr calc 'speedcrunch'
abbr color 'pastel format hex'
if exists /usr/bin/sdcv
  abbr D  'dict'
  abbr d  'sdcv -wn'
  abbr de 'sdcv -uk'
  abbr di 'sdcv -di'
  abbr th 'sdcv -th'
else
  abbr d 'dict'
  abbr di 'dict -t'
end
abbr dot 'rlwrap dotfiles'
abbr gif 'nsxiv -a'
abbr handbrake 'ghb'
abbr hex 'hexdump -C'
abbr kc "kconvert '*epub'"
abbr kindle 'dmenu econvert azw3'
abbr eformat 'dmenu econvert reformat'
abbr kobo 'dmenu econvert epub'
abbr md 'lowdown -tterm'
abbr mdtxt 'lowdown -tterm --term-no-colour'
abbr miniflux-migrate 'doas miniflux -c /etc/miniflux.conf -migrate'
abbr music '!p ncmpcpp ;and ncmpcpp'
abbr reqk 'qk stop; qk start'
abbr scrot 'scrot -e "mv \$f /net/photos/batchqueue/"'
abbr ss 'sc-im'
abbr td 'td -i'
# abbr todo 'rlwrap todo-screen'
abbr uterm 'urxvt -sh 1'
abbr W 'words'
abbr wl 'wc -l'
