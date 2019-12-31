function xbmc
    video xbmc
    cat ~/xbmc_crashlog-* >> ~/tmp/xbmc_crashlog.log 2>/dev/null
    rm -f ~/xbmc_crashlog-* 2>/dev/null
end

