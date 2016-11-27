# /etc/profile.d/bspwm.sh
# see .xinitrc -> . /etc/profile
[ $USER = root ] && exit
export BSPWM_HISTORY="/tmp/bspwm.history"
export BSPWM_SOCKET="/tmp/bspwm-socket"
export BSPWM_STACK="/tmp/bspwm.stack"
export BSPWM_TREE="/tmp/bspwm.tree"
export PANEL_FIFO="/tmp/panel-fifo"
export PANEL_HEIGHT="14"
export PATH="/usr/games:$HOME/.config/bspwm:$HOME/bin:$PATH"
export SHELL="/bin/sh"
export XDG_CONFIG_HOME="$HOME/.config"

