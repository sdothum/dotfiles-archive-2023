#!/bin/sh
# ebook library comparison: move all potentially new books to the calibre-compare folder

cd /net/media/ebooks
diff -qr Calibre\ Library Calibre\ Library.compare/ |sort > calibre.diff
#ack-grep \\.compare calibre.diff | grep '/: ' | sed -e 's,^.*/: ,,' -e 's,;,\\;,g' -e 's, ,\\ ,g' > author.diff
#ack-grep \\.compare calibre.diff | grep '//' | grep -v 'metadata.db differ' | sed -e 's,^.*//,,' -e 's,: ,/,g' -e 's, ,\\ ,g' > book.diff
ack-grep \\.compare calibre.diff | grep '/: ' | sed -e 's,^.*/: ,,' > author.sed
ack-grep \\.compare calibre.diff | grep '//' | grep -v 'metadata.db differ' | sed -e 's,^.*//,,' -e 's,: ,/,g' > book.sed
cat author.sed book.sed | sort > author-book.sort

mkdir calibre-compare 2>/dev/null
rm -f calibre-compare/* 2>/dev/null
> calibre-compare.log
/usr/bin/xterm -e "tail -f calibre-compare.log" &

while read folder
do
	echo ":: $folder ::" >>calibre-compare.log
	find "Calibre Library.compare/$(echo $folder | sed -e 's,;,\;,g' -e 's, ,\ ,g')" -name '*mobi' > mobi.find
	while read title
	do
		#echo ${title##*/} >>calibre-compare.log
		master=$(find "Calibre Library" -name "${title##*/}")
		if [ "$master." = . ]; then
			echo ${title##*/} >>calibre-compare.log
			cp "$title" calibre-compare/
		#else
		#	echo "-- match --" >>calibre-compare.log
		fi
	done < mobi.find
done < author-book.sort

rm -f author.sed book.sed author-book.sort mobi.find
