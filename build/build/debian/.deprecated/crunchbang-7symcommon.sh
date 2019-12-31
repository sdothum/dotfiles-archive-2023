#!/bin/bash
# symlink common config files that are tweaked with regularity

cd ~/Dropbox/debian/.config
for i in *; do
    # exclude a few folders that are not symlinked at the folder level (see crunchbang-8symlinks.sh) 
    if [ $(echo $i | egrep "autokey|fish|terminator"). = . ]; then  
        echo
        echo :: $i
        if [ -f ~/.config/$i -o -d ~/.config/$i ]; then
            if [ ! -f ~/.config/$i.save -a ! -d ~/.config/$i.save ]; then
                echo mv ~/.config/$i ~/.config/$i.save
                mv ~/.config/$i ~/.config/$i.save
            fi
        fi
        echo ln -sf ~/Dropbox/debian/.config/$i ~/.config/$i
        if [ -f ~/.config/$i ]; then
            rm -fv ~/.config/$i
        elif [ -d ~/.config/$i ]; then
            rm -Rfv ~/.config/$i
        fi
        ln -sf ~/Dropbox/debian/.config/$i ~/.config/$i
    fi
done
