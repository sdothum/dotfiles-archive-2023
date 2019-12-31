function vimwiki-mail
    # process incoming comments mail
    trace vimwiki-mail
    set pwd $PWD
    cd ~/vimwiki/mail
    # list of all articles (excluding alphabet index files)
    ls -1 ~/vimwiki/html/*.html | sed -e 's/.*html\///' -e  's/.html$//' | grep '...*' >rules/articles
    set emails (ls -1 ~/.mail/comments/The_Darnedest_Thing/new/* ~/.mail/comments/The_Darnedest_Thing/cur/*)

    for mail in $emails
        set post (basename $mail)
        trace 1 email $post
        # already processed once? search order by likelihood
        [ (f comments/ $post | wc -l) -gt 0 ]; and continue
        [ (f junk/ $post | wc -l) -gt 0 ]; and continue
        [ (f pending/ $post | wc -l) -gt 0 ]; and continue
        [ (f rejected/ $post | wc -l) -gt 0 ]; and continue
        [ (f empty/ $post | wc -l) -gt 0 ]; and continue

        vimwiki-post $mail
    end

    # remove empty article directories that may have resulted from vimwiki-post
    # or manual thunar processing and corrections of email submissions
    for dir in pending rejected empty
        find ~/vimwiki/mail/$dir -type d -empty -exec rmdir \{\} \;
    end
    cd $PWD
end
