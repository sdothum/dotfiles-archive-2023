function vimwiki-next
    # update the darnedest thing next article links
    trace vimwiki-next
    set pwd $PWD
    cd ~/vimwiki/html
    set next .(random)

    # the list of potential files and dated entries in reverse chronological order
    vimwiki-ls -r >$next.files

    # last article has no forwarding link
    set islast 1
    while read article
        # ignore stub pages
        [ (sed -n '/<!--start of wiki article-->/,/<!--end of wiki article-->/p' "$article" | wc -l) -lt 8 ]; and continue

        # set last article link
        if [ $islast = 1 ]
            set nextwiki "$article"
            set islast 0
            continue
        end

        # check if link needs updating (article may have been amended, hence, new relative date position)
        if [ (egrep -c "nextwiki.*$nextwiki" "$article") = 0 ]
            trace 1 wiki \"$article\" -> \"$nextwiki\"
            sed -n '1,/<!--end of wiki article-->/p' "$article" >$next.head
            # next wiki listing link
            echo "<p class=\"nextwiki\"><a href=\"$nextwiki\">→ "(basename "$nextwiki" .html)"</a></p>" >$next.body
            sed -n '/<!--next wiki article link-->/,$p' "$article" >$next.tail

            touch -r "$article" $next.timestamp
            # assemble completed wiki article with link
            cat $next.head $next.body $next.tail >"$article"
            # restore timestamp to retain chronological order
            touch -r $next.timestamp "$article"
        end
        set nextwiki "$article"
    end <$next.files

    command rm -f $next.head $next.body $next.tail $next.files $next.timestamp
    cd $PWD
end

