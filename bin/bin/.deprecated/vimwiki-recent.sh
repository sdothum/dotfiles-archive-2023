#!/bin/sh
# update the darnedest thing recent entries page
set -x

fileset=60
cd ~/vimwiki/html

# the list of potential files and dated entries
ls -t1 *.html | head -$fileset | egrep -v '^(date|index|recent|tag|the darnedest thing|todo).html' >files

rm -f dates 2>/dev/null
while read line; do
    # ignore other wiki articles
    if [ ! -f "../thedarnedestthing/${line%%.html}.wiki" ]; then
        continue
    fi

    date -r "$line" +%F >>dates
done <files
# file list is date sorted so just need cutoff trigger
if [ $(sort -u dates | head -7 | wc -l) = 7 ]; then
    cutoff=$(sort -u dates | head -7 | tail -1)
else
    cutoff="9999-99-99"
fi

# content div is used for index template/css <a href> per line
echo "        <div class=\"content\">" >body
date="0000-00-00"
while read line; do
    # ignore other wiki articles
    if [ ! -f "../thedarnedestthing/${line%%.html}.wiki" ]; then
        continue
    fi

    # looking for cutoff
    filedate=$(date -r "$line" +%F)
    if [ $filedate = $cutoff ]; then
        break
    fi

    sed -n '/<!--start of wiki article-->/,/<!--end of wiki article-->/p' "$line" >content
    # ignore stub pages
    if [ $(cat content | wc -l) -lt 8 ]; then
        continue
    fi

    # date heading
    if [ $filedate != $date ]; then
        date=$filedate
        echo "<h2>$(date -d $filedate '+%A %B %d, %Y' | tr '[:upper:]' '[:lower:]')</h2>" >>body
    fi

    # the following awk statement will only work if the <p> paragraph is a written as a single unbroken line, hence..
    #content=$(sed -n '/<p>/,/<\/p>/p' "$line" | sed -n '2,$p' | sed '/<\/p>/,$d' | awk '{ for (n=1; n<=10; n++) printf ("%s ", $n) }')
    # extract 1st paragraph
    content=$(sed -n '/<p>/,/<\/p>/p' "$line" | sed -n '2,$p' | sed '/<\/p>/,$d')
    # echo content as a single line string
    content=$(echo $content | awk '{ for (n=1; n<=45; n++) printf ("%s ", $n) }')
    # entry listing
    echo "<p><a href=\"$line\">${line%%.html}</a> <span class=\"opening\">$content</span> ...</p><hr>" >>body
done <files
echo "        </div>" >>body

sed -n '1,/<!--start of auto generated list-->/p' recent.html >head
sed -n '/<!--end of auto generated list-->/,$p' recent.html >tail

# pull the parts together :-)
cat head body tail >recent.html
rm -f head body tail content files dates

