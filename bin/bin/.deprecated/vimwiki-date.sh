#!/bin/sh
# update the darnedest thing date entries page
set -x

fileset=60
cd ~/vimwiki/html

# the list of potential files and dated entries
ls -t1 *.html | head -$fileset | egrep -v '^(date|index|recent|tag|the darnedest thing|todo).html' >files

# content div is used for index template/css <a href> per line
echo "        <div class=\"content\">" >body
date="0000-00-00"
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

    filedate=$(date -r "$line" +%F)
    # date heading
    if [ $filedate != $date ]; then
        date=$filedate
        echo "<h2>$(date -d $filedate '+%A %B %d, %Y' | tr '[:upper:]' '[:lower:]')</h2>" >>body
    fi

    # wiki listing
    echo "<a href=\"$line\">${line%%.html}</a>" >>body
done <files
echo "        </div>" >>body

sed -n '1,/<!--start of auto generated list-->/p' date.html >head
sed -n '/<!--end of auto generated list-->/,$p' date.html >tail

# pull the parts together :-)
cat head body tail >date.html
rm -f head body tail content files dates

