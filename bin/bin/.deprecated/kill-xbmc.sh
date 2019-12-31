#!/bin/sh
# manually manage xscreensaver

ps() { /bin/ps -ef | /bin/egrep "$1" | /bin/egrep -v "egrep|$0" ; }
pid() { ps $1 | /usr/bin/awk '{print $2}' ; }

if [ $(ps xbmc | /usr/bin/wc -l) -gt 0 ]; then
	echo "$(date '+%Y.%m.%d %H:%M:%D') --- killing xbmc ---" >>~/tmp/xbmc_crashlog.log 2>/dev/null
	ps xbmc >>~/tmp/xbmc_crashlog.log 2>/dev/null 
	/bin/kill -9 $(pid xbmc) >>~/tmp/xbmc_crashlog.log 2>/dev/null
fi
