function vimwiki-dates
    for file in (cat $argv)
        date -r "$file" +%F
    end
end
