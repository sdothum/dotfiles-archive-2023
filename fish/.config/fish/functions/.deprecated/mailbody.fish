function mailbody
    set mailbody .(random).mailbody
    command cp -f $argv $mailbody
    # strip mail header information
    grep -q '^Content-Transfer-Encoding: ' $mailbody; and sed -i -n '/^Content-Transfer-Encoding: /,$p' $mailbody
    grep -q '^User-Agent: ' $mailbody; and sed -i -n '/^User-Agent: /,$p' $mailbody
    sed -i -n '2,$p' $mailbody
    # strip signature
    grep -q '^--' $mailbody; and begin;
        sed -i -n '1,/^--/p' $mailbody
        set numlines (cat $mailbody | wc -l)
        sed -i -n '1,'(solve "$numlines - 1")'p' $mailbody
    end
    # fix linebreak anomalies from some mail clients and apply some simple formatting rules
    sed -i ':join;/=$/{N;s/=\n//;bjoin}' $mailbody
    sed -i -e 's/\t/ /' -e 's/  */ /g' -e 's/^ //' $mailbody
    # strip leading and trailing blank lines (as well as compressing multiple blank lines)
    grep -q '^[^ ]' $mailbody; and sed -i -n ''(grep -n '^[^ ]' $mailbody | head -1 | cut -d: -f1)',$p' $mailbody
    grep -q '^[^ ]' $mailbody; and sed -i -n '1,'(grep -n '^[^ ]' $mailbody | tail -1 | cut -d: -f1)'p' $mailbody
    cat -s $mailbody
    command rm -f $mailbody
end
