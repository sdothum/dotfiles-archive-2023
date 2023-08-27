
# Aliases
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# .................................................. Application file extensions

alias -s tex=vim
alias -s html=w3m
alias -s org=w3m

# ......................................................... Recursive expansions

alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ca="2>&1 | cat -A"
alias -g cat='sudo /usr/bin/cat'
alias -g c='clear'
alias -g close='eject -t'
alias -g wl='| wc -l'
alias -g d="DISPLAY=:0.0"
alias -g dn=/dev/null
alias -g ducks="du -cks * | sort -rn | egrep -v '^0|total'"
alias -g du='sudo du -h'
alias -g ed="export DISPLAY=:0.0"
alias -g eg='|& egrep'
alias -g egrep='sudo egrep'
alias -g eh='|& head'
alias -g el='|& less'
alias -g els='|& less -S'
alias -g etl='|& tail -20'
alias -g et='|& tail'
alias -g f=' | fmt -'
alias -g find='sudo find'
# alias -g g='| egrep'
alias -g g='| egrep --ignore-case --color'
alias -g gprename='command gprename $PWD'
alias -g gvim='vim -g'
alias -g handbrake='ghb'
alias -g h='| head'
alias -g hl='|& head -20'
alias -g iotop='command sudo iotop'
alias -g irb='command irb -I/usr/lib/ruby/1.9.1 -I/usr/lib/ruby/1.9.1/x86_64-linux'
# alias -g ll="2>&1 | less"
# alias -g l="| less"
alias -g ll='l -l'
alias -g locate='sudo locate'
# alias -g ls='| less -S'
alias -g ls='sudo /bin/ls --color -Fh'
alias -g l='ls -a'
alias -g lt='ll -t'
alias -g mdstat='watch -d cat /proc/mdstat'
alias -g m='| less'
alias -g mm='| most'
# alias -g m='| more'
alias -g music='!p ncmpcpp; and ncmpcpp'
alias -g mv='command mv -iv'
alias -g mysql='command mysql -h localhost -u root -p'
alias -g ne="2> /dev/null"
alias -g network-password='sudo nm-connection-editor'
alias -g ns='| sort -n'
alias -g nul="> /dev/null 2>&1"
alias -g nv='nvpy'
alias -g paclog='v /var/log/pacman.log'
alias -g path='echo $PATH'
alias -g pdf='evince'
alias -g pipe='|'
alias -g q='/usr/games/fortune'
alias -g radio='!p pyradio; and pyradio'
alias -g restart='sudo systemctl restart'
alias -g rns='| sort -nr'
alias -g r='> /tmp/tee.txt'
alias -g ruby='command ruby -I/usr/lib/ruby/1.9.1 -I/usr/lib/ruby/1.9.1/x86_64-linux'
alias -g sdiff='command sdiff -b -E -W -w(tput cols)'
alias -g sk="*~(*.bz2|*.gz|*.tgz|*.zip|*.z)"
alias -g s='| sort'
alias -g start='sudo systemctl start'
alias -g status='sudo systemctl status'
alias -g stop='sudo systemctl stop'
alias -g su='command sudo su'
alias -g syslog='sudo journalctl -b'
alias -g tl='| tail -20'
alias -g todo='rlwrap todo-screen'
alias -g tree='command tree -ACF'
alias -g t='| tail'
alias -g t='top -n 1'
alias -g ups='sudo pwrstat -test; sleep 3; sudo pwrstat -config; sudo pwrstat -status'
alias -g usb='sudo ls -l /dev/disk/by-id/*usb*'
alias -g us='| sort -u'
alias -g vf='vifm;'
alias -g vm=/var/log/messages
alias -g v='vim -g'
alias -g www='/usr/bin/w3m (grep -v 'w3m/MANUAL.html'~/.config/w3m/history | tail -1);'
alias -g x0g='| xargs -0 egrep'
alias -g x0='| xargs -0'
alias -g xg='| xargs egrep'
alias -g xp='xprop | grep "WM_WINDOW_ROLE\\|WM_CLASS"; and echo "WM_CLASS(STRING) = \"NAME\", \"CLASS\""'
# alias -g x='| xargs'
