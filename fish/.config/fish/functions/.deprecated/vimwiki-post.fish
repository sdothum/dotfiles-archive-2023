function vimwiki-post
    # process user comment
    set pwd $PWD
    cd ~/vimwiki/mail
    set post (basename "$argv")
    trace 3 [vimwiki-post] file $post

    # take care of spammers and trolls
    if [ (keywords "$argv" rules/spammers | grep '^From: ' | wc -l) -gt 0 ]
        trace 1 "[vimwiki-post] junk -> spammers "(keywords "$argv" rules/spammers | grep '^From: ')
        touch junk/$post
        return
    end

    # screen out invalid subject headings (can't match to site article otherwise)
    set subject (keywords "$argv" rules/articles | egrep '^Subject: /// .* ///' | sed 's/^Subject: \/\/\/ \(.*\) \/\/\//\1/')
    trace 2 [vimwiki-post] wiki \"$subject\"
    if [ "$subject" = "" ]
        trace 1 "[vimwiki-post] rejected -> "(grep '^Subject: ' "$argv")
        touch rejected/$post
        return
    end

    function mkdir
        if [ ! -d "$argv" ]
            trace 2 [vimwiki-post] mkdir "$argv/"
            command mkdir  -p "$argv"
        end
    end

    # extract body of email
    mkdir "pending/$subject"
    mailbody "$argv" | sed 's/^\(.*\)$/<p class="comment">\1<\/p>/' >"pending/$subject/$post"
    set comment (sed -n '1,$p' "pending/$subject/$post")
    if [ "$comment" = '<p class="comment"></p>' ]
        trace 1 "[vimwiki-post] empty -> Subject: $subject"
        mkdir "empty/$subject"
        command mv "pending/$subject/$post" "empty/$subject/"
        return
    end

    # is the extracted content free of questionable formatting?
    if [ (mailscan "pending/$subject/$post" | wc -l) -gt 0 ]
        trace 1 "[vimwiki-post] pending -> Subject: $subject"
        return
    end

    # good comment! add header to it
    trace 1 "[vimwiki-post] comments -> Subject: $subject"
    set date (date --date=""(grep "^Date: " "$argv" | head -1 | sed 's/^Date: \(..., [0-9]* ... [0-9]*\) .*$/\1/')"" '+%B %d, %Y')
    sed -i "1i<p class=\"date\">$date<\/p>" "pending/$subject/$post"
    mkdir "comments/$subject"
    command mv "pending/$subject/$post" "comments/$subject/"
    cd $PWD
end
