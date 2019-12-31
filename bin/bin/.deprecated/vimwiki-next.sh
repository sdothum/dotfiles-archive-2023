#!/bin/sh
# update the darnedest thing next article links
set -x

fileset=60
cd ~/vimwiki/html

# the list of potential files and dated entries in reverse chronological order
ls -t1r *.html | head -$fileset | egrep -v '^(date|index|recent|tag|the darnedest thing|todo).html' >files

# last article has no forwarding link
islast=1
while read line; do
    # ignore other wiki articles
    if [ ! -f "../thedarnedestthing/${line%%.html}.wiki" ]; then
        continue
    fi

    sed -n '/<!--start of wiki article-->/,/<!--end of wiki article-->/p' "$line" >content
    # ignore stub pages
    if [ $(cat content | wc -l) -lt 8 ]; then
        continue
    fi

    # set last article link
    if [ $islast = 1 ]; then
        nextwiki="$line"
        islast=0
        continue
    fi

    # check if link needs updating (article may have been amended, hence, new relative date position)
    if [ $(egrep -c "nextwiki.*$nextwiki" "$line") = 0 ]; then
        sed -n '1,/<!--end of wiki article-->/p' "$line" >head
        # next wiki listing link
        echo "<p class=\"nextwiki\"><a href=\"$nextwiki\">→ ${nextwiki%%.html}</a></p>" >body
        sed -n '/<!--next wiki article link-->/,$p' "$line" >tail

        touch -r "$line" timestamp
        # assemble completed wiki article with link
        cat head body tail >"$line"
        # restore timestamp to retain chronological order
        touch -r timestamp "$line"
    fi
    nextwiki="$line"
done <files

rm -f head body tail content files timestamp

