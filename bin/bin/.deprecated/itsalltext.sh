#!/bin/sh
# focuswriter patch for itsalltext
set -eu
echo "::: $(date '+%Y.%m.%d %l:%M:%S%P') ::: $@" >>~/tmp/itsalltext.log 2>/dev/null
cat $@ >>~/tmp/itsalltext.log 2>/dev/null
#exec /usr/bin/focuswriter "$@"
exec /usr/bin/geany "$@"
