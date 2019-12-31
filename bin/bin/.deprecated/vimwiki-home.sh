#!/bin/sh
# update the darnedest thing home page
set -x

fileset=30
cd ~/vimwiki/html

# the list of potential files and last dated entry
ls -t1 *.html | head -$fileset | egrep -v '^(date|index|recent|tag|the darnedest thing|todo).html' >files
first=$(head -1 files)
date=$(date -r "$first" +%F)

rm -f body 2>/dev/null
separator=0
while read line; do
    # ignore other wiki articles
    if [ ! -f "../thedarnedestthing/${line%%.html}.wiki" ]; then
        continue
    fi

    # looking for the most recent activity
    if [ $(date -r "$line" +%F) = $date ]; then
        sed -n '/<!--start of wiki article-->/,/<!--end of wiki article-->/p' "$line" >content
        # ignore stub pages
        if [ $(cat content | wc -l) -ge 8 ]; then
            # separate topics
            if [ $separator = 1 ]; then
                echo "        <hr>" >>body
            fi
            separator=1
            cat content >>body
        fi
    else
        # have reached older files
        break
    fi
done <files

sed -n '1,/<!--start of auto generated list-->/p' the\ darnedest\ thing.html >head
sed -n '/<!--end of auto generated list-->/,$p' the\ darnedest\ thing.html >tail

# pull the parts together :-)
cat head body tail >the\ darnedest\ thing.html
rm -f head body tail content files

