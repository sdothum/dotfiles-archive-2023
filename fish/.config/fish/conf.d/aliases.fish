# sdothum - 2016 (c) wtfpl

# Fish Shell
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ...................................................................... Package

if which pacaur >/dev/null ^&1
  alias am 'pacaur -Ss'
  alias AM 'pacaur -Ss | less'
else
  alias am 'yaourt -Ss'
  alias AM 'yaourt -Ss --pager'
end

alias pkgs 'comm -23 <(pacman -Qeq | sort) <(pacman -Qgq base base-devel | sort)'
alias pn 'env D=N pd'
alias pq 'pacman -Qii'
alias ts "egrep -vi '%|--:--|<=>|\'.*\' -> \'.*\'|\[[ -co]*\]|/usr/|^(\+\+* |../../|/bin/| * (or)*c[cp](ld)* * | * gen * |automake: |cd | *check(ing)* |config.status: |configure.ac|configure: |copying |done |for | *from|g*cc |g-ir-scanner: |gpg: |in |installing |libtool(ize)*: |make |make\[.\] |making |mv |patching |recieving objects: |remote: |resolving deltas: |rm |tests/check|touch )' ~/typescript | less"
alias tse "grep '> \(ERROR\|WARNING\)' ~/typescript"

# ...................................................................... Process

alias K 'env sig=-KILL k'
alias KK 'env sig=-KILL kk'
alias sv 'systemd'
alias svd 'systemd disable'
alias sve 'systemd enable'
alias svl 'systemd reload'
alias svm 'systemd mask'
alias svq 'systemd status'
alias svr 'systemd restart'
alias svs 'systemd start'
alias svt 'systemd stop'
alias svu 'systemd unmask'

# ....................................................................... Device

alias close 'eject -t'
alias left_shift_key 'test -f /etc/X11/Xmodmap ;and xmodmap /etc/X11/Xmodmap'
alias mount 'sudo mount'
alias umount 'sudo umount'

# ....................................................................... System

alias blame 'systemd-analyze blame'
alias font-manager 'font-manager ; sudo rm -f ~/.fonts.conf'
alias fontmatrix 'fontmatrix ; sudo rm -f ~/.fonts.conf'
alias gtop 'glances'
alias iotop 'sudo iotop'
alias lpr 'lpr -P hp-LaserJet-1320-series'
alias screenfetch 'fetch'
alias services "systemctl list-units -t service --no-pager --no-legend | grep active | egrep -v 'systemd|exited' | cut -d' ' -f1"
alias traceroute 'mtr --report -c 1'
alias who 'command w'

# ...................................................................... Network

alias friendlybear 'smbclient -R bcast //friendlybear/patricia motu om'

# ......................................................................... File

alias cp 'cp -i'
alias cpl 'cp -iLRfv'
alias cpv 'cp -iv'
alias gprename 'gprename $PWD'
alias m 'less'
alias mv 'mv -i'
alias mvv 'mv -iv'
alias n 'env VISUAL=vim nnn'
alias r 'vifm'
alias R 'env VISUAL=vim ranger'
alias rm 'rm -i'
alias rmv 'rm -iv'
alias t 'tail -f'

# .................................................................... Directory

alias dus "/usr/bin/du -hs * | sort -h"
alias l1 'ls -1'
alias ldot 'ls -lAd .*'
alias ll 'ls -lA'
alias llr 'ls -lAR'
alias lr 'ls -LAR'
alias lt 'll -t'
alias path 'echo $PATH'
alias pp 'pwd'
alias tree 'sudo tree -aCF'
alias treed 'sudo tree -aCdF'

# ....................................................................... Search

alias fd 'find -type d'
alias ff 'find -type f'
alias f 'find -iname'
# alias g 'ack --ignore-case'
# alias gl 'ack -l --ignore-case'
alias locate 'sudo locate'
alias mgrep 'pcregrep -r -M'
alias w 'which'

# ...................................................................... Desktop

alias c 'clear ; setterm -cursor on'
alias cursor 'setterm -cursor on'
alias h: 'ls -l /tmp/herbstluftwm:*'
alias hc 'herbstclient'
alias herbstluftwm ". $HOME/.config/herbstluftwm/config/ENV"
alias X x

# ......................................................................... Edit

alias de 'dmenu - edit'
alias dp 'dmenu - projects'
alias dr 'dmenu - run'
alias ds 'dmenu - scripts'
alias nv 'nvpy'
alias vd 'rm -f $HOME/.session/follow_the_sun ; gvimdiff --role=gvimdiff'
alias sd 'sdiff -b -E -W -w(tput cols)'
alias vdarchive 'dirdiff ./ /net/archive(pp)'
alias vdbackup 'dirdiff ./ /net/backup(pp)'

# .................................................................. Development
 
# alias ghc 'ghc -dynamic'  # arch repo only
alias ghc 'stack ghc'
alias ghcc 'stack build'
alias ghcx 'stack runghc'
alias ghci 'stack exec ghci'
alias git1 'git clone --depth 1'
alias mysql 'mysql -h localhost -u root -p'

# .................................................................. Application

alias calc 'speedcrunch'
alias handbrake 'ghb'
alias music '!p ncmpcpp ;and ncmpcpp'
alias scrot 'scrot -e "mv \$f /net/photos/batchqueue/"'
alias ss 'sc-im'
alias todo 'rlwrap todo-screen'
alias uterm 'urxvt -sh 1'
