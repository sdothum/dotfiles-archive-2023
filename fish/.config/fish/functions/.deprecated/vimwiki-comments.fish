function vimwiki-comments
    # generate the darnedest thing commented articles
    trace vimwiki-comments
    set pwd $PWD
    cd ~/vimwiki
    set comments .(random)

    # the list of commented articles
    find mail/comments -maxdepth 1 -type d | sed 's/^mail\/comments\///' >$comments.dirs
    # no comments yet?
    [ (cat $comments.dirs | wc -l) -eq 1 ]; and return
    sed -i -n '2,$p' $comments.dirs
    trace 2 queue (sed 's/^/\/ /' $comments.dirs)

    # concatenate the comments to the original article
    while read article
        trace 1 wiki \"$article\"
        # ignore stub pages
        [ (sed -n '/<!--start of wiki article-->/,/<!--end of wiki article-->/p' "html/$article.html" | wc -l) -lt 8 ]; and continue

        # pull the parts together :-)
        cd "mail/comments/$article"; cat * | sed '1i<hr>\r' >../../../$comments.body; cd -
        # revert possible "show comments" link (don't want the show comments page so show a link to itself!)
        sed -i 's/<p class="showcomments">.*●\&nbsp\&nbsp\&nbsp/<p class="showcomments">/' "html/$article.html"
        sed -n '1,/<!--start of auto generated list-->/p' "html/$article.html" >$comments.head
        sed -n '/<!--end of comments-->/,$p' "html/$article.html" >$comments.tail
        # put parts together and correct links to point to root folder
        cat $comments.head $comments.body $comments.tail | sed -e 's/\(<a href="\)/\1..\//g' -e 's/\(<a href="\)..\/\(mailto:\)/\1\2/g' -e 's/\(<a href="\)..\/\(http:\)/\1\2/g' >"html/comments/$article.html"

        # update show comments link in master page
        sed -i 's/<p class="showcomments">/<p class="showcomments"><a href="comments\/'$article'.html">show comments<\/a>\&nbsp\&nbsp\&nbsp●\&nbsp\&nbsp\&nbsp/' "html/$article.html"
    end <$comments.dirs

    command rm -f $comments.head $comments.body $comments.tail $comments.dirs
    cd $PWD
end

