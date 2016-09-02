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
alias ts "egrep -v '%|--:--|<=>' ~/typescript | less"
alias tse "grep '> \(ERROR\|WARNING\)' ~/typescript"

# ...................................................................... Process
 
alias K 'env sig=-KILL k'
alias KK 'env sig=-KILL kk'
alias restartd 'systemd restart'
alias startd 'systemd start'
alias statusd 'systemd status'
alias stopd 'systemd stop'

# ....................................................................... Device
 
alias close 'eject -t'
alias left_shift_key 'test -f /etc/X11/Xmodmap ;and xmodmap /etc/X11/Xmodmap'
alias mount 'sudo mount'
alias umount 'sudo umount'

# ....................................................................... System
 
alias htop 'glances'
alias iotop 'sudo iotop'
alias screenfetch 'fetch'
alias traceroute 'mtr --report -c 1'

# ...................................................................... Desktop
 
alias c 'clear ;and setterm -cursor on'
alias cursor 'setterm -cursor on'
alias h: 'ls -l /tmp/herbstluftwm:*'
alias hc 'herbstclient'
alias herbstluftwm ". $HOME/.config/herbstluftwm/config/ENV"

# .................................................................... Directory
 
alias l1 'ls -1'
alias ldot 'ls -lAd .*'
alias ll 'ls -lA'
alias llr 'ls -lAR'
alias locate 'sudo locate'
alias lpr 'lpr -P hp-LaserJet-1320-series'
alias lr 'ls -LAR'
alias lt 'll -t'
alias path 'echo $PATH'
alias pp 'pwd'
alias stow 'stow -t ~ -v'
alias tree 'sudo tree -aCF'
alias treed 'sudo tree -aCdF'

# ................................................................... Filesystem
 
alias cp 'cp -i'
alias cpl 'cp -iLRfv'
alias cpv 'cp -iv'
alias df 'command df ;and sudo btrfs filesystem df /'
alias ducks "du -cks * | sort -rn | egrep -v '^0|total'"
alias dud 'du -d 1 -h'
alias gprename 'gprename $PWD'
alias m 'less'
alias mgrep 'pcregrep -M'
alias mv 'mv -i'
alias mvv 'mv -iv'
alias rm 'rm -i'
alias rmv 'rm -iv'
alias t 'tail -f'

# ....................................................................... Search
 
alias fd 'find . -type d'
alias ff 'find . -type f'
alias f 'find . -iname'
alias g 'ack --ignore-case'
alias gl 'ack -l --ignore-case'

# ......................................................................... Edit
 
alias nv 'nvpy'
alias sd 'sdiff -b -E -W -w(tput cols)'
alias vd 'gvimdiff --role=gvimdiff'

# .................................................................. Application

alias calc 'speedcrunch'
alias font-manager 'font-manager ;and sudo rm -f ~/.fonts.conf'
alias fontmatrix 'fontmatrix ;and sudo rm -f ~/.fonts.conf'
alias handbrake 'ghb'
alias music '!p ncmpcpp ;and ncmpcpp'
alias mysql 'mysql -h localhost -u root -p'
alias scrot 'scrot -e "mv \$f /net/photos/batchqueue/"'
alias ss 'sc-im'
alias todo 'rlwrap todo-screen'
alias uterm 'urxvt -sh 1'
alias we 'wego'
