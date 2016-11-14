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
alias disabled 'systemd disable'
alias enabled 'systemd enable'
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

alias font-manager 'font-manager ; sudo rm -f ~/.fonts.conf'
alias fontmatrix 'fontmatrix ; sudo rm -f ~/.fonts.conf'
alias htop 'glances'
alias iotop 'sudo iotop'
alias lpr 'lpr -P hp-LaserJet-1320-series'
alias path 'echo $PATH'
alias screenfetch 'fetch'
alias traceroute 'mtr --report -c 1'

# ...................................................................... Network

alias friendlybear 'smbclient //friendlybear/patricia motu om'

# ................................................................... Filesystem

alias df "command df -h ; echo ; sudo btrfs filesystem df / ; echo ; sudo btrfs filesystem du -s '/home /opt /usr /var'"
alias ducks "du -cks * | sort -rn | egrep -v '^0|total'"
alias dud 'du -d 1 -h'

# ......................................................................... File

alias cp 'cp -i'
alias cpl 'cp -iLRfv'
alias cpv 'cp -iv'
alias gprename 'gprename $PWD'
alias m 'less'
alias mv 'mv -i'
alias mvv 'mv -iv'
alias rm 'rm -i'
alias rmv 'rm -iv'
alias stow 'stow -t ~ -v'
alias t 'tail -f'

# .................................................................... Directory

alias l1 'ls -1'
alias ldot 'ls -lAd .*'
alias ll 'ls -lA'
alias llr 'ls -lAR'
alias lr 'ls -LAR'
alias lt 'll -t'
alias pp 'pwd'
alias tree 'sudo tree -aCF'
alias treed 'sudo tree -aCdF'

# ....................................................................... Search

alias fd 'find . -type d'
alias ff 'find . -type f'
alias f 'find . -iname'
alias g 'ack --ignore-case'
alias gl 'ack -l --ignore-case'
alias locate 'sudo locate'
alias mgrep 'pcregrep -M'

# ...................................................................... Desktop

alias c 'clear ; setterm -cursor on'
alias cursor 'setterm -cursor on'
alias h: 'ls -l /tmp/herbstluftwm:*'
alias hc 'herbstclient'
alias herbstluftwm ". $HOME/.config/herbstluftwm/config/ENV"
alias X x

# ......................................................................... Edit

alias nv 'nvpy'
alias sd 'sdiff -b -E -W -w(tput cols)'
alias vd 'gvimdiff --role=gvimdiff'

# .................................................................. Application

alias calc 'speedcrunch'
alias handbrake 'ghb'
alias music '!p ncmpcpp ;and ncmpcpp'
alias mysql 'mysql -h localhost -u root -p'
alias scrot 'scrot -e "mv \$f /net/photos/batchqueue/"'
alias ss 'sc-im'
alias todo 'rlwrap todo-screen'
alias uterm 'urxvt -sh 1'
alias we 'wego'
