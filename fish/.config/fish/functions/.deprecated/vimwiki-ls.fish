function vimwiki-ls
    # the list of potential files and dated entries in reverse chronological order
    for file in (ls -t $argv *.html | egrep -v '^([a-z0-9]|date|index|recent|rss|tag|the darnedest thing|todo).html')
        # ignore other wiki articles
        [ ! -f ../thedarnedestthing/(basename "$file" .html).wiki ]; and continue
        echo $file
    end
end
