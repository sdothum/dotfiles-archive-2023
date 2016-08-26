# sdothum - 2016 (c) wtfpl

# Fish Shell
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ........................................................... Package management

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
alias ts "test -f typescript ;and egrep -v '%|--:--|<=>' typescript | less ;or egrep -v '%|--:--|<=>' ~/typescript | less"
alias tse "grep '> \(ERROR\|WARNING\)' ~/typescript"

# ....................................................................... System
 
alias K 'env sig=-KILL k'
alias KK 'env sig=-KILL kk'
alias mount 'sudo mount'
alias restartd 'systemd restart'
alias startd 'systemd start'
alias statusd 'systemd status'
alias stopd 'systemd stop'
# alias su 'sudo su'
# alias syslog 'sudo journalctl -b'
alias umount 'sudo umount'

# ....................................................................... Device
 
# alias ata 'sudo /usr/bin/ls -l /dev/disk/by-id/*ata*'
# alias label 'sudo /usr/bin/ls -l /dev/disk/by-label/*'
# alias usb 'sudo /usr/bin/ls -l /dev/disk/by-id/*usb*'
# alias uuid 'sudo /usr/bin/ls -l /dev/disk/by-uuid/*'

alias close 'eject -t'
alias left_shift_key 'test -f /etc/X11/Xmodmap ;and xmodmap /etc/X11/Xmodmap'
alias iotop 'sudo iotop'
alias traceroute 'mtr --report -c 1'

# ...................................................................... Desktop
 
# alias c 'clear ;and setterm -cursor on'
alias cursor 'setterm -cursor on'
alias h: 'ls -l /tmp/herbstluftwm:*'
alias hc 'herbstclient'
alias herbstluftwm ". $HOME/.config/herbstluftwm/config/ENV"

# .................................................................... Directory
 
# alias j 'fasd_cd -d'
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
alias fd 'find . -type d'
alias ff 'find . -type f'
alias f 'find . -iname'
alias g 'ack --ignore-case'
alias gl 'ack -l --ignore-case'
alias gprename 'gprename $PWD'
alias m 'less'
alias mgrep 'pcregrep -M'
alias mv 'mv -i'
alias mvv 'mv -iv'
alias rm 'rm -i'
alias rmv 'rm -iv'
alias t 'tail -f'

# ......................................................................... Edit
 
alias gvim 'vim -g'
alias gvimdiff "vimdiff -g --role='gvimdiff'"
alias nv 'nvpy'
alias sdiff 'sdiff -b -E -W -w(tput cols)'
# alias sed 'sed -r'
alias vd 'gvimdiff --role=gvimdiff'
# alias vi 'v'
# alias vf 'vifm'

# .................................................................. Application

alias calc 'speedcrunch'
alias font-manager 'font-manager ;and sudo rm -f ~/.fonts.conf'
alias fontmatrix 'fontmatrix ;and sudo rm -f ~/.fonts.conf'
alias handbrake 'ghb'
alias music '!p ncmpcpp ;and ncmpcpp'
alias mysql 'mysql -h localhost -u root -p'
# alias rip 'rrip_gui >/dev/null ^&1 &'
alias scrot 'scrot -e "mv \$f /net/photos/batchqueue/"'
alias ss 'sc-im'
alias todo 'rlwrap todo-screen'
alias uterm 'urxvt -sh 1'
alias we 'wego'
