
# Aliases
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# .................................................. Application file extensions

alias -s ace='unace l'
alias -s chm=xchm
alias -s djvu=djview
alias -s dvi=xdvi
alias -s pdf=acroread
alias -s ps=gv
alias -s rar='unrar l'
alias -s tar.gz='echo '
alias -s tar='tar tf'
alias -s zip='unzip -l'

map() {
  default=$1
  shift
  for i in $@
  do
    alias -s $i=$default
  done
}

map $BROWSER htm html de org net com at cx nl se dk dk php

map $EDITOR cpp cxx cc c hh h inl asc txt TXT tex

map $XIVIEWER jpg jpeg png gif mng tiff tif xpm

map $PLAYER ape avi flv mkv mov mp3 mpeg mpg ogg ogm rm wav webm

# ........................................................... Global definitions

alias -g ...='../..'
alias -g ....='../../..'
alias -g AA='|& ack'
alias -g ack="sudo ack --smart-case --color --color-match='bold yellow' --color-filename='bold red'"
alias -g AZ='[a-zA-Z]*'
# alias -g cat='sudo /usr/bin/cat'
alias -g DD='DISPLAY=:0.0'
alias -g DN=/dev/null
alias -g egrep='sudo egrep --color -E'
alias -g find='sudo find'
alias -g GG='|& egrep --ignore-case --color -E'
alias -g head='sudo head'
alias -g HH='|& head -50'
alias -g LL='|& less'
# alias -g less='sudo less'
alias -g ls='sudo /bin/ls --color -Fh'
alias -g MM='|& less'
alias -g more='sudo more'
alias -g NE='2>/dev/null'
alias -g NUL='2>&1 >/dev/null'
alias -g TE='> /tmp/tee.txt'
alias -g SS='| sort'
alias -g tail='sudo tail'
alias -g TT='|& tail -50'
alias -g VM=/var/log/messages
alias -g WL='|& wc -l'
alias -g XD='export DISPLAY=:0.0'
alias -g ZZ="|& egrep -v '\.deprecated|\.hg|\.vimv|vimmappings'"

# .................................................................... Shortcuts

# alias a='ack'
# alias al='ack -l'
if which pacaur >/dev/null 2>&1; then
  alias am='pacaur -Ss'
  alias AM='pacaur -Ss | less'
else
  alias am='yaourt -Ss'
  alias AM='yaourt -Ss --pager'
fi 
# alias ata='sudo /usr/bin/ls -l /dev/disk/by-id/*ata*'
alias c='clear && setterm -cursor on'
alias calc='speedcrunch'
alias cd='cd -q'
alias close='eject -t'
alias cpl='cp -LRfv'
alias cpv='cp -v'
alias cursor='setterm -cursor on'
alias demon='systemd'
# alias df='sudo btrfs filesystem df'
alias ducks="du -cks * | sort -rn | egrep -v '^0|total'"
alias dud='du -d 1 -h'
alias fd='find . -type d'
alias ff='find . -type f'
alias f='find . -iname'
alias font-manager='font-manager && sudo rm -f ~/.fonts.conf'
alias fontmatrix='fontmatrix && sudo rm -f ~/.fonts.conf'
alias g='egrep --ignore-case -E'
alias gl='egrep -l --ignore-case -E'
alias gprename='gprename $PWD'
alias gvim='vim -g'
alias gvimdiff="vimdiff -g --role='gvimdiff'"
alias h:='ls -l /tmp/herbstluftwm:*'
alias handbrake='ghb'
alias hc='herbstclient'
alias herbstluftwm=". $HOME/.config/herbstluftwm/config/ENV"
alias hh='h HH'
alias iotop='sudo iotop'
# alias j='fasd_cd -d'
alias K='sig=-KILL k'
alias KK='sig=-KILL kk'
alias l1='ls -1'
# alias label='sudo /usr/bin/ls -l /dev/disk/by-label/*'
alias ldot='ls -lAd .*'
alias left_shift_key='[[ -f /etc/X11/Xmodmap ]] && xmodmap /etc/X11/Xmodmap'
alias ll='ls -lA'
alias llr='ls -lAR'
# alias l='ls -A'
alias locate='sudo locate'
alias lpr='lpr -P hp-LaserJet-1320-series'
alias lr='ls -LAR'
alias lt='ll -t'
alias m='less'
alias mgrep='pcregrep -M'
alias mount='sudo mount'
alias music='!p ncmpcpp && ncmpcpp'
alias mvv='mv -v'
alias mysql='mysql -h localhost -u root -p'
alias nop="true '"
alias nul='cat /dev/null >'
alias nv='nvpy'
alias p1='rm -f $HOME/.zprezto/.prompt'
alias p2='touch $HOME/.zprezto/.prompt'
alias path='echo $PATH'
# alias peanuts='dmenu alarm 20m "Peanuts at 275F" 6'
alias peanuts='dmenu alarm 20m "Peanuts at 400F with cooldown in oven" 1'
alias pkgs='comm -23 <(pacman -Qeq | sort) <(pacman -Qgq base base-devel | sort)'
alias pn='D=N pd'
alias pp='pwd'
alias pq='pacman -Qii'
# alias radio='!p pyradio && pyradio'
alias rmv='rm -v'
# alias roxterm='SHELL=/usr/bin/zsh roxterm'
alias restart='systemd restart'
# alias rip='rrip_gui >/dev/null 2>&1 &'
# alias sakura='SHELL=/usr/bin/zsh sakura'
alias scrot='scrot -e "mv \$f /net/photos/batchqueue/"'
alias sdiff='sdiff -b -E -W -w$(tput cols)'
# alias sed='sed -r'
alias ss='sc-im'
alias start='systemd start'
alias status='systemd status'
alias stop='systemd stop'
alias stow='stow -t ~ -v'
# alias su='sudo su'
# alias syslog='sudo journalctl -b'
alias t='tail -f'
alias traceroute='mtr --report -c 1'
alias tree='sudo tree -aCF'
alias treed='sudo tree -aCdF'
alias ts="[[ -f typescript ]] && egrep -v '%|--:--|<=>' typescript | less || egrep -v '%|--:--|<=>' ~/typescript | less"
alias tse="grep '> \(ERROR\|WARNING\)' ~/typescript"
alias umount='sudo umount'
# alias usb='sudo /usr/bin/ls -l /dev/disk/by-id/*usb*'
# alias uuid='sudo /usr/bin/ls -l /dev/disk/by-uuid/*'
alias uterm='urxvt -sh 1'
alias vd='gvimdiff --role=gvimdiff'
# alias vi='v'
# alias vf='vifm'
alias we='wego'

# ............................................ Disable prezto function conflicts

if alias d >/dev/null ;then
  # unalias cp
  unalias d
  unalias l
  # unalias mv
  unalias p
  # unalias rm
  unalias top
fi
