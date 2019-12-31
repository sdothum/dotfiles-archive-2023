function vimwiki-date
    # update the darnedest thing date entries page
    trace vimwiki-date
    set pwd $PWD
    cd ~/vimwiki/html
    set date .(random)

    # the list of potential files and dated entries
    vimwiki-ls >$date.files

    # content div is used for index template/css <a href> per line
    echo "        <div class=\"content\">" >$date.body
    set currentdate 0000-00-00
    while read article
        # ignore stub pages
        [ (sed -n '/<!--start of wiki article-->/,/<!--end of wiki article-->/p' "$article" | wc -l) -lt 8 ]; and continue

        set filedate (date -r "$article" +%F)
        # date heading
        if [ $filedate != $currentdate ]
            trace 1 date (date -r "$article" +%F)
            set currentdate $filedate
            echo \<h2\>(date -d $filedate '+%B %d, %Y' | tr '[:upper:]' '[:lower:]')\</h2\> >>$date.body
        end

        trace 1 wiki \"$article\"
        # wiki listing
        echo "<a href=\"$article\">"(basename "$article" .html)"</a>" >>$date.body
    end <$date.files
    echo "        </div>" >>$date.body

    sed -n '1,/<!--start of auto generated list-->/p' date.html >$date.head
    sed -n '/<!--end of auto generated list-->/,$p' date.html >$date.tail

    # pull the parts together :-)
    cat $date.head $date.body $date.tail >date.html
    command rm -f $date.head $date.body $date.tail $date.files
    cd $PWD
end

