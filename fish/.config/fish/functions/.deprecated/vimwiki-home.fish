function vimwiki-home
    # update the darnedest thing home page
    trace vimwiki-home
    set pwd $PWD
    cd ~/vimwiki/html
    set home .(random)

    # insert preface into home page
    sed -n '1,/<!--start of wiki content-->/p' the\ darnedest\ thing.html >$home.head
    vimwiki-preface >$home.content
    sed -n '/<!--end of wiki content-->/,$p' the\ darnedest\ thing.html >$home.tail
    cat $home.head $home.content $home.tail >the\ darnedest\ thing.html

    # the list of potential files and last dated entry
    vimwiki-ls >$home.files
    set first (head -1 $home.files)
    set date (date -r "$first" +%F)

    set separator 0
    while read article
        # looking for the most recent activity
        if [ (date -r "$article" +%F) = $date ]
            trace 1 wiki \"$article\"
            #sed -n '/<!--start of wiki article-->/,/<!--end of wiki article-->/p' "$article" >$home.content
            sed -n '/<!--start of wiki article-->/,/<p class="showcomments">/p' "$article" >$home.content
            # ignore stub pages
            #if [ (cat $home.content | wc -l) -ge 8 ]
            if [ (cat $home.content | wc -l) -ge 14 ]
                # separate topics
                #[ $separator = 1 ]; and echo "        <hr>" >>$home.body
                set separator 1
                cat $home.content >>$home.body
            end
        else
            # have reached older files
            break
        end
    end <$home.files

    sed -n '1,/<!--start of auto generated list-->/p' the\ darnedest\ thing.html >$home.head
    sed -n '/<!--end of auto generated list-->/,$p' the\ darnedest\ thing.html >$home.tail

    # pull the parts together :-)
    cat $home.head $home.body $home.tail >the\ darnedest\ thing.html
    command rm -f $home.head $home.body $home.tail $home.content $home.files
    cd $PWD
end

