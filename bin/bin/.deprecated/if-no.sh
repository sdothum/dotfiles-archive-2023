#!/bin/sh
# conditional shell execution
# /usr/local/bin/if-no "comment" command...
#set -x
echo
echo "-> $1"
shift
while [ 1 ]; do
    read -p "continue? [no]/yes :" CONTINUE
    if [ "$CONTINUE." = "y." -o "$CONTINUE." = "Y." ]; then
        $@
        exit
    elif [ "$CONTINUE." = "." -o "$CONTINUE." = "n." -o "$CONTINUE." = "N." ]; then
        exit
    fi
done
