#!/bin/bash
set -x
[ ! -f /usr/bin/fish ] && sudo aptitude install fish
mkdir -p ~/.config/fish 2>/dev/null

chsh --shell /usr/bin/fish

if [ $HOME = /root ]; then
    ln -sf /home/shum/.config/fish/config.fish ~/.config/fish/
    if [ ! -f ~/.bashrc.original ]; then
        cp ~/.bashrc ~/.bashrc.original
    fi
    ln -sf /home/shum/.bashrc ~/
    ln -sf /home/shum/.bash_aliases ~/

    # user shell
    su - shum -c "chsh --shell /usr/bin/fish"
fi

