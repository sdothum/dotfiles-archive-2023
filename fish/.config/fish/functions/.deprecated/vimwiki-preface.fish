function vimwiki-preface
    # fetch the darnedest thing home page preface section
    trace 8 vimwiki-preface
    set preface .(random)

    # the list of potential files and last dated entry
    ls ~/vimwiki/html/diary/*html >$preface.files

    while read article
        # looking for the most recent activity
        trace 9 wiki \"$article\"
        sed -n '/<!--start of wiki content-->/,/<!--end of wiki content-->/p' "$article" | sed -n '2,$p' | sed '/<!--end of wiki content-->/,$d' >$preface.content
        # ignore stub pages
        if [ (cat $preface.content | wc -l) -ge 8 ]
            cat $preface.content
            break
        end
    end <$preface.files

    command rm -f $preface.files $preface.content
end

