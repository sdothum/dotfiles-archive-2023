#!/bin/sh
#    recently_opened_menu.sh - a script to parse .recently-used.xbel
#    and generate openbox pipe menu
#    Copyright (C) 2010  John Crawley
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

# Usage: add
# <menu id="recent" label="Recent Files" execute="/path/to/recently_opened_menu.sh" />
# to your .config/openbox/menu.xml, or use with dash_places_menu.sh (see comments there)

maximum_entries=15 # max. number of entries in menu

#######################################################################

# if argument is --clear, empty .recently-used.xbel
[ "$1" = '--clear' ] && {
cat <<':EOF' > "${HOME}"/.recently-used.xbel
<?xml version="1.0" encoding="UTF-8"?>
<xbel version="1.0"
      xmlns:bookmark="http://www.freedesktop.org/standards/desktop-bookmarks"
      xmlns:mime="http://www.freedesktop.org/standards/shared-mime-info"
>
</xbel>
:EOF
exit
}

maximum_entries=$((maximum_entries+2))

pre='    <item label="'
mid='">
    <action name="Execute"><command>'
post='</command></action>
    </item>'
    
files=$( tac "${HOME}"/.recently-used.xbel |  awk -v MAX="$maximum_entries" -v PR="$pre" -v MI="$mid" -v PO="$post" 'BEGIN {
    RS="</bookmark>";
    FS="<info>";
}
(NR == MAX) {exit}
!/<bookmark/ {next}
# $1 is the command, $2 the file path
{
    sub(/^.*exec=\"\&apos\;/,"",$1)
    sub(/\&apos\;.*$/,"",$1)
    sub(/ *%./,"",$1)
    sub(/^.*file:\/\//,"",$2)
    sub(/\".*$/,"",$2)
    gsub(/%22/,"\&quot;",$2)
    gsub(/%3C/,"\&lt;",$2)
    gsub(/%3E/,"\&gt;",$2)
    name=$2
    sub(/^.*\//,"",name)
    gsub(/\&apos;/,"\&apos;\&quot;\&apos;\&quot;\&apos;",$2)
    print (PR name MI $1 " '"'"'" $2 "'"'"'" PO)
}' )  

# use perl to decode urlencoded characters
files=$(perl -MURI::Escape -e 'print uri_unescape($ARGV[0]);' "$files")

output='<openbox_pipe_menu>
'"$files"'
<separator />
    <item label="Clear Recent Files">
        <action name="Execute">
            <command>
            &apos;'"$0"'&apos; --clear
            </command>
        </action>
    </item>
</openbox_pipe_menu>
'
printf '%s' "$output"  # printf because echo sometimes eats backslashes
