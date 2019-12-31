#!/bin/sh
# conditional shell execution
# /usr/local/bin/if-yes "comment" command...
#set -x
echo
echo "-> $1"
shift
while [ 1 ]; do
    read -p "continue? [yes]/no :" CONTINUE
    if [ "$CONTINUE." = "." -o "$CONTINUE." = "y." -o "$CONTINUE." = "Y." ]; then
        $@
        exit
    elif [ "$CONTINUE." = "n." -o "$CONTINUE." = "N." ]; then
        exit
    fi
done
