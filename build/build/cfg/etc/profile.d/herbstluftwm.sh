# /etc/profile.d/herbstluftwm.sh
# see .xinitrc -> . /etc/profile
[ $USER = root ] && exit
export PATH=$HOME/.config/herbstluftwm:$HOME/bin:$PATH
export SHELL=/bin/sh
export XDG_CONFIG_HOME=$HOME/.config
