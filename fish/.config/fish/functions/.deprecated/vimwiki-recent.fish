function vimwiki-recent
    # update the darnedest thing recent entries page
    trace vimwiki-recent
    set pwd $PWD
    cd ~/vimwiki/html
    set recent .(random)

    # the list of potential files and dated entries
    vimwiki-ls >$recent.files
    vimwiki-dates $recent.files >$recent.dates

    # file list is date sorted so just need cutoff trigger
    [ (sort -u $recent.dates | head -7 | wc -l) = 7 ]; and set cutoff (sort -u $recent.dates | head -7 | tail -1); or set cutoff 9999-99-99

    # content div is used for index template/css <a href> per line
    echo "        <div class=\"content\">" >$recent.body
    set currentdate 0000-00-00
    while read article
        # ignore other wiki articles
        [ ! -f ../thedarnedestthing/(basename "$article" .html).wiki ]; and continue
        # looking for cutoff
        set filedate (date -r "$article" +%F)
        [ $filedate = $cutoff ]; and break

        sed -n '/<!--start of wiki article-->/,/<!--end of wiki article-->/p' "$article" >$recent.content
        # ignore stub pages
        [ (cat $recent.content | wc -l) -lt 8 ]; and continue

        # date heading
        if [ $filedate != $currentdate ]
            trace 1 currentdate $filedate
            set currentdate $filedate
            echo \<h2\>(date -d $filedate '+%B %d, %Y' | tr '[:upper:]' '[:lower:]')\</h2\> >>$recent.body
        end

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
        # entry listing
        #echo "<p><a href=\"$article\">"(basename "$article" .html)"</a> <span class=\"opening\">$content</span> ...</p><hr>" >>$recent.body
        echo "<p><span class=\"article\"><a href=\"$article\">"(basename "$article" .html)"</a></span> <span class=\"opening\">$content</span> ...</p>" >>$recent.body
    end <$recent.files
    echo "        </div>" >>$recent.body

    sed -n '1,/<!--start of auto generated list-->/p' recent.html >$recent.head
    sed -n '/<!--end of auto generated list-->/,$p' recent.html >$recent.tail

    # pull the parts together :-)
    cat $recent.head $recent.body $recent.tail >recent.html
    command rm -f $recent.head $recent.body $recent.tail $recent.content $recent.files $recent.dates
    cd $PWD
end

