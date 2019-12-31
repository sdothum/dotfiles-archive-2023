function vimwiki-rss
    # update the darnedest thing recent entries page
    trace vimwiki-rss
    set fileset 60
    set pwd $PWD
    cd ~/vimwiki/html
    set rss .(random)

    # the list of potential files and dated entries
    vimwiki-ls | head -$fileset >$rss.files
    vimwiki-dates $rss.files >$rss.dates

    # file list is date sorted so just need cutoff trigger
    [ (sort -u $rss.dates | head -7 | wc -l) = 7 ]; and set cutoff (sort -u $rss.dates | head -7 | tail -1); or set cutoff 9999-99-99

    # content div is used for index template/css <a href> per line
    while read article
        # ignore other wiki articles
        [ ! -f ../thedarnedestthing/(basename "$article" .html).wiki ]; and continue
        # looking for cutoff
        set filedate (date -r "$article" +%F)
        [ $filedate = $cutoff ]; and break

        sed -n '/<!--start of wiki article-->/,/<!--end of wiki article-->/p' "$article" >$rss.content
        # ignore stub pages
        [ (cat $rss.content | wc -l) -lt 8 ]; and continue

        trace 1 wiki \"$article\"
        # extract the paragraph content first, then..
        set content (sed -n '/<p>/,/<\/p>/p' "$article" | sed -n '2,$p' | sed '/<\/p>/,$d')
        # no paragragh? then look for verse..
        if [ "$content" = "" ]
            set content (sed -n '/<pre class="verse">/,/<\/pre>/p' "$article" | sed -n '2,$p' | sed '/<\/pre>/,$d')
        end
        # or headers
        if [ "$content" = "" ]
            # set content (grep '<h[2-6] .*>' "$article" | head -1 | sed -e 's/<.*>\(.*\)<.*>/\1/')
            set content (grep '<h[2-6] .*>' "$article" | sed -e 's/<.*>\(.*\)<.*>/\1,/')
        end
        # echo content to pipe as a single string to awk
        set content (echo "$content" | awk '{ for (n=1; n< 45; n++) printf ("%s ", $n) }')

        # item listing
        echo "<item>" >>$rss.body
        grep "<title>" $article | sed 's/.*\(<title>.*\)/\1/' >>$rss.body
        echo "<description>"(echo $content | sed 's/ *$//')"</description>" >>$rss.body
        echo "<link>http://thedarnedestthing.com/"(echo $article | sed 's/ /%20/')"</link>" >>$rss.body
        echo "<guid>http://thedarnedestthing.com/"(echo $article | sed 's/ /%20/')"</guid>" >>$rss.body
        echo "<pubDate>"(date --rfc-822 -r $article)"</pubDate>" >>$rss.body
        echo "</item>" >>$rss.body
    end <$rss.files

    sed -n '1,/<!--start of rss items-->/p' rss.html | sed 's/<lastBuildDate>.*<\/lastBuildDate>/<lastBuildDate>'(date --rfc-822)'<\/lastBuildDate>/' >$rss.head
    sed -n '/<!--end of rss items-->/,$p' rss.html >$rss.tail

    # pull the parts together :-)
    cat $rss.head $rss.body $rss.tail >rss.xml
    command rm -f $rss.head $rss.body $rss.tail $rss.content $rss.files $rss.dates
    cd $PWD
end

