#!/bin/sh

# X11
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ........................................................................ dmenu

# dmenu restart/launch init.d process

# add proxy script to list :-)
cd /etc/init.d/
# file=$(ls | sed '$aproxy' | sort | $(dwrapper) -p 'Restart:')
file=$(stest -flx . | sort | sed '1iproxy' | $(dwrapper) -p 'Restart:')
[[ $file ]] && zsh -c "restart $file"
