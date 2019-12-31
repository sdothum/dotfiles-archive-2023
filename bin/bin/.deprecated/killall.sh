#!/bin/sh
# regexp killall
# /usr/local/bin/killall.sh regexp
#set -x
ps() { /bin/ps -ef | /bin/egrep "$1" | /bin/egrep -v "egrep|$0" ; }
pid() { ps $1 | /usr/bin/awk '{print $2}' ; }

if [ ! "$1." = "." ]; then
	if [ $(ps $1 | /usr/bin/wc -l) -gt 0 ]; then
		ps $1
		if-continue.sh "kill /$1/ $(echo $(pid $1))" /bin/kill -9 $(pid $1)
	fi
fi
