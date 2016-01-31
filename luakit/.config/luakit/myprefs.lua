-- =================================================================================
-- setting to enable reasonably long cookie persistence
require "cookies"
cookies.session_timeout = 60*60*24*365
cookies.store_session_cookies = true

-- my keymap extensions
local key, buf, but = lousy.bind.key, lousy.bind.buf, lousy.bind.but

add_binds("normal", {

    key({"Mod4"}, "Page_Down", "Scroll page down.",
        function (w)
          w:set_mode("passthrough")
          --w:scroll{ ypagerel =  1.0 }
        end),

    key({"Mod4"}, "Page_Up", "Scroll page up.",
        function (w)
          w:set_mode("passthrough")
          --w:scroll{ ypagerel = -1.0 }
        end),

    key({}, ";", "Enter `command` mode.",
        function (w) w:set_mode("command") end),

    key({}, "b", "Go back in the browser history.",
        function (w, m) w:back(m.count) end),

    key({}, "m", "Go forward in the browser history.",
        function (w, m) w:forward(m.count) end),

    key({"Control"}, "p", "Print current page.",
        function (w) w.view:eval_js("print()") end),

    buf("^yy$", "Yank current URI to primary selection and clipboard.",
        function (w)
            local uri = string.gsub(w.view.uri or "", " ", "%%20")
            luakit.selection.primary = uri
            luakit.selection.clipboard = uri
            w:notify("Yanked uri: " .. uri)
        end),

    buf("^yt$", "Yank current title to primary selection and clipboard.",
        function (w)
            local title = w.view.title
            luakit.selection.primary = title
            luakit.selection.clipboard = title
            w:notify("Yanked title: " .. title)
        end),

    buf("^ys$", "Yank current selection to clipboard.",
        function (w)
            local text = luakit.selection.primary
            if not text then w:error("Empty selection.") return end
            luakit.selection.clipboard = text
            w:notify("Yanked selection: " .. text)
            luakit.selection.primary = ""
        end),

    key({"Shift"}, "F11", "Toggle tablist/statusbar mode.",
        -- see function window.init_funcs.hide_tablist_fullscreen (in rc.lua)
        function (w)
            my_show_tablist = not my_show_tablist
            if my_show_tablist then
                w.tablist.widget:show()
                w.sbar.ebox:show()
                w.sbar.hidden = false
            else
                w.tablist.widget:hide()
                w.sbar.ebox:hide()
                w.sbar.hidden = true
            end
        end),

    key({"Control"}, "F11", "Toggle fullscreen mode.",
        -- see function window.init_funcs.hide_tablist_fullscreen (in rc.lua)
        function (w)
            w.win.fullscreen = not w.win.fullscreen
            if w.win.fullscreen then
                if my_show_tablist then w.tablist.widget:show() end
            else
                w.tablist.widget:hide()
            end
        end),

    buf("^g5$", "Youtube html5 setting.",
        function (w) w:new_tab("http://www.youtube.com/html5") end),

    -- buf("^gB$", "Open bittorrent sync.",
    --     function (w) w:new_tab("http://localhost:8888/gui/en/index.html") end),

    buf("^gB$", "Open bookmarks.",
        function (w) w:new_tab("luakit://bookmarks/") end),

    buf("^gb$", "Browse bookmarks",
        function (w) w:browse_bookmarks_dmenu() end),

    buf("^gC$", "Open CUPS server.",
        function (w) w:new_tab("http://localhost:631/") end),

    buf("^gc$", "Open CouchPotato.",
        function (w) w:new_tab("http://localhost:5050/") end),

    -- buf("^ge$", "Open 8tracks.",
    --     function (w) w:new_tab("http://8tracks.com/") end),

    buf("^gF$", "Open weathernetwork hourly weather forecast.",
        function (w) w:new_tab("http://www.theweathernetwork.com/hourly-weather-forecast/canada/ontario/ottawa") end),

    buf("^gf$", "Open weathernetwork forecast.",
        function (w) w:new_tab("http://www.theweathernetwork.com/weather/canada/ontario/ottawa") end),

    -- buf("^gf$", "Open accuweather forecast.",
    --     function (w) w:new_tab("http://www.accuweather.com/en/ca/ottawa/k1s/current-weather/55487") end),

    buf("^gH$", "Open history.",
        function (w) w:new_tab("luakit://history/") end),

    buf("^gh$", "Browse url history",
        function (w) w:browse_history_dmenu() end),

    -- buf("^gj$", "Open jango.",
    --     function (w) w:new_tab("http://www.jango.com/profiles/54787964?l=0") end),

    --buf("^gK$", "Open nzbgeeks forum.",
    --    function (w) w:new_tab("https://forums.nzbgeek.info/") end),

    buf("^gK$", "Open nzbgeeks preindex.",
        function (w) w:new_tab("https://01100111011001010110010101101011.info/preindex") end),

    buf("^gk$", "Open nzbgeek movies.",
        function (w) w:new_tab("https://01100111011001010110010101101011.info/search/nzbgeek?t=2040") end),

    buf("^gm$", "Open gmail.",
        function (w) w:new_tab("https://mail.google.com/mail/u/0/?shva=1#inbox") end),

    buf("^gN$", "Open nzbget.",
        function (w) w:new_tab("http://localhost:6789/nzbget:nzbget/") end),

    buf("^gn$", "Open NzbDrone.",
        function (w) w:new_tab("http://localhost:8989/") end),

    -- buf("^gP$", "Open privoxy.",
    --     function (w) w:new_tab("http://config.privoxy.org/") end),

    buf("^gP$", "Select proxy.",
        function (w) w:enter_cmd(":proxy ") end),

    buf("^gp$", "Open Plex media server.",
       function (w) w:new_tab("http://localhost:32400/web/index.html#!/dashboard") end),

    -- buf("^gR$", "Open tiny tiny rss preferences.",
    --     function (w) w:new_tab("http://luna:8000/tt-rss/prefs.php") end),
    --
    -- buf("^gr$", "Open tiny tiny rss.",
    --     function (w) w:new_tab("http://luna:8000/tt-rss/") end),

    buf("^gS$", "Open syncthing.",
        function (w)
          w:new_tab("http://luna:8080/")
          w:new_tab("http://monad:8080/")
        end),

    buf("^gs$", "Open syncthing.",
        function (w)
          w:new_tab("http://localhost:8080/")
        end),

    -- buf("^gs$", "Open SickBeard.",
    --     function (w) w:new_tab("http://localhost:8081/comingEpisodes/") end),

    -- buf("^gs$", "Open Songza.",
    --     function (w) w:new_tab("http://songza.com/") end),

    -- buf("^gT$", "Open tiny tiny rss.",
    --     function (w) w:new_tab("http://luna:8000/tt-rss/") end),

    buf("^gT$", "Open tiny tiny rss preferences.",
        function (w) w:new_tab("http://localhost:8000/tt-rss/prefs.php") end),

    buf("^gt$", "Open tiny tiny rss.",
        function (w) w:new_tab("http://localhost:8000/tt-rss/") end),

    buf("^gU$", "Open usenet-crawler forum.",
        function (w) w:new_tab("http://www.usenet-crawler.com/forum/") end),

    buf("^gu$", "Open usenet-crawler movies.",
        --function (w) w:new_tab("http://www.usenet-crawler.com/movies?t=2040") end),
        function (w) w:new_tab("http://www.usenet-crawler.com/search/yify?t=2040") end),

    buf("^gW$", "Open thedarnedestthing.com.",
        function (w, c) w:enter_cmd(":open http://thedarnedestthing.com/admin/do%20:") end),

    buf("^gw$", "Open thedarnedestthing.",
        function (w) w:new_tab("http://thedarnedestthing.com/the%20darnedest%20thing") end),

    --buf("^gX$", "Open xbmc.",
    --    function (w) w:new_tab("http://localhost:8080") end),
    buf("^gX$", "Open plexmediaserver.",
        function (w) w:new_tab("http://localhost:32400/web/index.html#!/dashboard") end),

    buf("^gx$", "Translate page.",
        function (w) w:google_translate() end),

    buf("^gZ$", "Open newztown forum.",
        function (w) w:new_tab("http://newztown.co.za/forum/") end),

    buf("^gz$", "Open newztown movies.",
        function (w) w:new_tab("https://www.newztown.co.za/movies?t=2040") end),

    key({}, "F8", "Open localhost://thedarnedestthing/.",
        function (w) w:new_tab("http://thedarnedestthing:8000/") end),

    key({"Shift"}, "F8", "Open localhost://thedarnedestthing/resources.",
        function (w) w:new_tab("http://thedarnedestthing:8000/resources") end),

    key({"Control"}, "F8", "Admin localhost://thedarnedestthing/.",
        function (w, c) w:enter_cmd(":open http://thedarnedestthing:8000/admin/do%20:") end),

    key({}, "F7", "List thedarnedestthing draft articles.",
        function (w) w:new_tab("http://thedarnedestthing:8000/admin/do%20:draft") end),

    key({"Shift"}, "F7", "List thedarnedestthing orphan articles.",
        function (w) w:new_tab("http://thedarnedestthing:8000/admin/do%20:orphan") end),

    key({"Shift"}, "F4", "My google translate.",
        function (w)
            w.view:eval_js("function(){var%20t=((window.getSelection&&window.getSelection())||(document.getSelection&&document.getSelection())||(document.selection&&document.selection.createRange&&document.selection.createRange().text));var%20e=(document.charset||document.characterSet);if(t!=''){location.href='http://translate.google.com/translate_t?text='+t+'&hl=en&langpair=auto|en&tbb=1&ie='+e;}else{location.href='http://translate.google.com/translate?u='+escape(location.href)+'&hl=en&langpair=auto|en&tbb=1&ie='+e;};;}()")
        end),

    key({}, "F4", "My readability script.",
        function (w)
            -- http://readable.tastefulwords.com/
            w.view:eval_js("(function(){_readableOptions={'text_font':'Helvetica Neue, Arial, DejaVu Sans, Verdana, Geneva, sans-serif','text_font_monospace':'Inconsolata','text_font_header':'Helvetica, quote(Helvetica Neuve), Arial, Tahoma, sans-serif','text_size':'14px','text_line_height':'1.7','box_width':'37em','color_text':'#FDF6E3','color_background':'#1B1D1E','color_links':'#99CCFF','text_align':'normal','base':'blueprint','custom_css':''};if(document.getElementsByTagName('body').length>0);else{return;}if(window.$readable){if(window.$readable.bookmarkletTimer){return;}}else{window.$readable={};}window.$readable.bookmarkletTimer=true;window.$readable.options=_readableOptions;if(window.$readable.bookmarkletClicked){window.$readable.bookmarkletClicked();return;}_readableScript=document.createElement('script');_readableScript.setAttribute('src','http://readable-static.tastefulwords.com/target.js?rand='+encodeURIComponent(Math.random()));document.getElementsByTagName('body')[0].appendChild(_readableScript);})()")
            -- w:set_mode("passthrough")
            w:set_mode("normal")
        end),

    key({}, "F5", "My lastpass fill.",
        function (w)
          w.view:eval_js("(function(){/*AutoFill_LastPass*/_LPG=function(i){return%20document.getElementById(i);};_LPT=function(i){return%20document.getElementsByTagName(i);};if(_LPG('_lpiframe')){_LPG('_lpiframe').parentNode.removeChild(_LPG('_lpiframe'));}if(_LPG('_LP_RANDIFRAME')){_LPG('_LP_RANDIFRAME').parentNode.removeChild(_LPG('_LP_RANDIFRAME'));}_LASTPASS_INC=function(u,s){if(u.match(/_LASTPASS_RAND/)){alert('Cancelling_request_may_contain_randkey');return;}s=document.createElement('script');s.setAttribute('type','text/javascript');s.setAttribute('src',u);if(typeof(window.attachEvent)!='undefined'){if(_LPT('body').length){_LPT('body').item(0).appendChild(s);}else{_LPT('head').item(0).appendChild(s);}}else{if(_LPT('head').length){_LPT('head').item(0).appendChild(s);}else{_LPT('body').item(0).appendChild(s);}}};_LASTPASS_INC('https://lastpass.com/bml.php'+String.fromCharCode(63)+'v=0&a=0&r='+Math.random()+'&h=b37fedb1028bba2403f0fd3da4ad3849e4f2b9228b27efd52c01beaa71d0b05c&u='+escape(document.location.href));_LPM=function(m){var%20targetFrame=_LPG(m.data.frame);if(null!=targetFrame&&typeof(targetFrame)!='undefined'&&typeof(targetFrame.contentWindow)!='undefined')targetFrame.contentWindow.postMessage(m.data,'*');};if(window.addEventListener){window.addEventListener('message',_LPM,false);}else{window.attachEvent('onmessage',_LPM);}var%20t=document.createElement('iframe');t.setAttribute('id','_LP_RANDIFRAME');t.setAttribute('sandbox','allow-scripts');t.frameBorder='0';t.setAttribute('src','https://lastpass.com/bml.php?u=1&hash=1&gettoken=0&donotcache=13998561141910563938');t.setAttribute('onload',%22document.getElementById('_LP_RANDIFRAME').contentWindow.postMessage('60dae8607d4c1b6b8cd6716b9631e65264d25c60b0a05a740a8635727b07267b','*');%22);if(typeof(window.attachEvent)!='undefined'){if(_LPT('body').length){_LPT('body').item(0).appendChild(t);}else{document.getElementByTagName('head').item(0).appendChild(t);}}else{if(_LPT('head').length){_LPT('head').item(0).appendChild(t);}else{_LPT('body').item(0).appendChild(t);}}})()")
        end),

    key({"Shift"}, "F5", "My lastpass fill form.",
        function (w)
          w.view:eval_js("(function(){/*Fill_Forms_LastPass*/_LPG=function(i){return%20document.getElementById(i);};_LPT=function(i){return%20document.getElementsByTagName(i);};if(_LPG('_lpiframe')){_LPG('_lpiframe').parentNode.removeChild(_LPG('_lpiframe'));}if(_LPG('_LP_RANDIFRAME')){_LPG('_LP_RANDIFRAME').parentNode.removeChild(_LPG('_LP_RANDIFRAME'));}_LASTPASS_INC=function(u,s){if(u.match(/_LASTPASS_RAND/)){alert('Cancelling_request_may_contain_randkey');return;}s=document.createElement('script');s.setAttribute('type','text/javascript');s.setAttribute('src',u);if(typeof(window.attachEvent)!='undefined'){if(_LPT('body').length){_LPT('body').item(0).appendChild(s);}else{_LPT('head').item(0).appendChild(s);}}else{if(_LPT('head').length){_LPT('head').item(0).appendChild(s);}else{_LPT('body').item(0).appendChild(s);}}};_LASTPASS_INC('https://lastpass.com/bml.php'+String.fromCharCode(63)+'v=0&f=1&r='+Math.random()+'&h=b37fedb1028bba2403f0fd3da4ad3849e4f2b9228b27efd52c01beaa71d0b05c&u='+escape(document.location.href));_LPM=function(m){var%20targetFrame=_LPG(m.data.frame);if(null!=targetFrame&&typeof(targetFrame)!='undefined'&&typeof(targetFrame.contentWindow)!='undefined')targetFrame.contentWindow.postMessage(m.data,'*');};if(window.addEventListener){window.addEventListener('message',_LPM,false);}else{window.attachEvent('onmessage',_LPM);}var%20t=document.createElement('iframe');t.setAttribute('id','_LP_RANDIFRAME');t.setAttribute('sandbox','allow-scripts');t.frameBorder='0';t.setAttribute('src','https://lastpass.com/bml.php?u=1&hash=1&gettoken=0&donotcache=13998562571003958487');t.setAttribute('onload',%22document.getElementById('_LP_RANDIFRAME').contentWindow.postMessage('60dae8607d4c1b6b8cd6716b9631e65264d25c60b0a05a740a8635727b07267b','*');%22);if(typeof(window.attachEvent)!='undefined'){if(_LPT('body').length){_LPT('body').item(0).appendChild(t);}else{document.getElementByTagName('head').item(0).appendChild(t);}}else{if(_LPT('head').length){_LPT('head').item(0).appendChild(t);}else{_LPT('body').item(0).appendChild(t);}}})()")
        end),

    key({"Control"}, "F5", "My lastpass login.",
        function (w)
          w.view:eval_js("(function(){/*AutoLogin_LastPass*/_LPG=function(i){return%20document.getElementById(i);};_LPT=function(i){return%20document.getElementsByTagName(i);};if(_LPG('_lpiframe')){_LPG('_lpiframe').parentNode.removeChild(_LPG('_lpiframe'));}if(_LPG('_LP_RANDIFRAME')){_LPG('_LP_RANDIFRAME').parentNode.removeChild(_LPG('_LP_RANDIFRAME'));}_LASTPASS_INC=function(u,s){if(u.match(/_LASTPASS_RAND/)){alert('Cancelling_request_may_contain_randkey');return;}s=document.createElement('script');s.setAttribute('type','text/javascript');s.setAttribute('src',u);if(typeof(window.attachEvent)!='undefined'){if(_LPT('body').length){_LPT('body').item(0).appendChild(s);}else{_LPT('head').item(0).appendChild(s);}}else{if(_LPT('head').length){_LPT('head').item(0).appendChild(s);}else{_LPT('body').item(0).appendChild(s);}}};_LASTPASS_INC('https://lastpass.com/bml.php'+String.fromCharCode(63)+'v=0&a=1&r='+Math.random()+'&h=b37fedb1028bba2403f0fd3da4ad3849e4f2b9228b27efd52c01beaa71d0b05c&u='+escape(document.location.href));_LPM=function(m){var%20targetFrame=_LPG(m.data.frame);if(null!=targetFrame&&typeof(targetFrame)!='undefined'&&typeof(targetFrame.contentWindow)!='undefined')targetFrame.contentWindow.postMessage(m.data,'*');};if(window.addEventListener){window.addEventListener('message',_LPM,false);}else{window.attachEvent('onmessage',_LPM);}var%20t=document.createElement('iframe');t.setAttribute('id','_LP_RANDIFRAME');t.setAttribute('sandbox','allow-scripts');t.frameBorder='0';t.setAttribute('src','https://lastpass.com/bml.php?u=1&hash=1&gettoken=0&donotcache=1399855647510140078');t.setAttribute('onload',%22document.getElementById('_LP_RANDIFRAME').contentWindow.postMessage('60dae8607d4c1b6b8cd6716b9631e65264d25c60b0a05a740a8635727b07267b','*');%22);if(typeof(window.attachEvent)!='undefined'){if(_LPT('body').length){_LPT('body').item(0).appendChild(t);}else{document.getElementByTagName('head').item(0).appendChild(t);}}else{if(_LPT('head').length){_LPT('head').item(0).appendChild(t);}else{_LPT('body').item(0).appendChild(t);}}})()")
        end),

    key({"Shift","Control"}, "C", "CouchPotato script.",
        function (w)
            w.view:eval_js("(function(){var e=document.createElement('script');e.setAttribute('type','text/javascript');e.setAttribute('charset','UTF-8');e.setAttribute('src','http://localhost:5050/api/e3efc040085d4255b82c98ad71b9aef4/userscript.bookmark/?host=http://localhost:5050/api/e3efc040085d4255b82c98ad71b9aef4/userscript.get/eeyB9mFX/&r='+Math.random()*99999999);document.body.appendChild(e);})()")
        end),

    key({"Shift","Control"}, "P", "Plex it! script.",
        function (w)
            w.view:eval_js("(function(){var s=document.createElement('script');s.type='text/javascript';s.src='https://my.plexapp.com/queue/bookmarklet_payload?uid=d88c87119ebae585';var h=document.getElementsByTagName('head')[0];h.appendChild(s);void(0);})()")
        end),

    key({"Shift","Control"}, "M", "Add to my delicious account.",
        function (w)
            -- w:set_mode("passthrough")
            w:set_mode("insert")
            w.view:eval_js("(function(){location.href='http://del.icio.us/post?v=3&url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title);})()")
        end),

    key({"Shift"}, "Return", "Blocking external editor.",
        function (w)
            local s = w.view:eval_js("document.activeElement.value")
            local n = "/home/shum/tmp/" .. os.time() .. ".mkd"
            local f = io.open(n, "w")
            f:write(s)
            f:flush()
            f:close()
            local l = io.open("/home/shum/tmp/wiki.log", "a")
            l:write(s)
            l:flush()
            l:close()

            -- cannot use xfce4-terminal (detaches from spawning process)
            --luakit.spawn_sync("/usr/bin/xfce4-terminal -e '/usr/bin/vim -c \"set spell\" " .. n .. " '")
            --luakit.spawn_sync("/usr/bin/terminator -e '/usr/bin/nano " .. n .. " '")
            luakit.spawn_sync("/usr/bin/xfce4-terminal -e '/usr/bin/vim -g -f -c \"set formatoptions=twan1 textwidth=72\" -c \"set nocp spell wrap\" " .. n .. " '")

            f = io.open(n, "r")
            s = f:read("*all")
            f:close()
            -- Strip the string
            s = s:gsub("^%s*(.-)%s*$", "%1")
            -- Escape it but remove the quotes
            s = string.format("%q", s):sub(2, -2)
            -- lua escaped newlines (slash+newline) into js newlines (slash+n)
            s = s:gsub("\\\n", "\\n")
            w.view:eval_js("document.activeElement.value = '" .. s .. "'")
            os.remove(n)
        end),

    key({"Control"}, "Return", "Non-blocking external editor.",
        function (w)
            --local editor = "/usr/bin/terminator -e '/usr/bin/nano" 
            local t = os.time()
            local n = "/home/shum/tmp/" .. t .. ".mkd"
            local marker = "luakit_extedit_" .. t

            local function editor_callback(exit_reason, exit_status)
                f = io.open(n, "r")
                s = f:read("*all")
                f:close()
                -- Strip the string
                s = s:gsub("^%s*(.-)%s*$", "%1")
                -- Escape it but remove the quotes
                s = string.format("%q", s):sub(2, -2)
                -- lua escaped newlines (slash+newline) into js newlines (slash+n)
                s = s:gsub("\\\n", "\\n")

                w.view:eval_js(string.format([=[
                    var e = document.getElementsByClassName('%s');
                    if(1 == e.length && e[0].disabled){
                        e[0].focus();
                        e[0].value = "%s";
                        e[0].disabled = false;
                        e[0].className = e[0].className.replace(/\b %s\b/,'');
                    }
                ]=], marker, s, marker))
                os.remove(n)
            end

            local s = w.view:eval_js(string.format([=[
                var e = document.activeElement;
                if(e && (e.tagName && 'TEXTAREA' == e.tagName || e.type && 'text' == e.type)){
                    var s = e.value;
                    e.className += " %s";
                    e.disabled = true;
                    e.value = '%s';
                    s;
                }else 'false';
            ]=], marker, n))
            if "false" ~= s then
                local f = io.open(n, "w")
                f:write(s)
                f:flush()
                f:close()
                local l = io.open("/home/shum/tmp/wiki.log", "a")
                l:write(s)
                l:flush()
                l:close()
                --luakit.spawn("/usr/bin/terminator -e '/usr/bin/vim -c \"set spell\" " .. n .. " '", editor_callback)
                luakit.spawn("/usr/local/bin/retext " .. n, editor_callback)
            end
        end),

}, true)


-- enable scrollbars
webview.init_funcs.show_scrollbars = function(view)
    --view.show_scrollbars = true
    view.show_scrollbars = false
end

-- fullscreen mode w/o tabs (see updated F11 in binds.lua)
window.init_funcs.hide_tablist_fullscreen = function (w)
    w.win:add_signal("updated", function ()
        if w.win.fullscreen then w.tablist.widget:hide() end
    end)
end

-- make all new windows tabs instead
webview.init_funcs.window_decision = function (view, w)
    view:add_signal("new-window-decision", function (v, uri, reason)
        w:new_tab(uri)
        return true
    end)
end

-- handle mailto links
webview.init_funcs.mailto_hook = function (view, w)
    view:add_signal("navigation-request", function (v, uri)
        if string.match(uri, "^mailto:") then
            --local cmd = string.format("%s %q", "xterm -T mailto -e mutt", uri)
            local cmd = string.format("%s %q", "geary", uri)
            luakit.spawn(cmd)
            return false
        end
    end)
end

-- downloads.default_dir = os.getenv("HOME") .. "/down"
downloads.default_dir = "/data/downloads/http"

-- save last session
local close_win = window.methods.close_win
window.methods.close_win = function (w, ...)
    session.save{w}
    close_win(w, ...)
end

-- magnet: hook
webview.init_funcs.magnet_hook = function (view, w)
    view:add_signal("navigation-request", function (v, uri)
        if string.match(uri, "^magnet:") then
            local cmd = string.format("%s %q", "transmission-gtk", uri)
            luakit.spawn(cmd)
            return false
        end
    end)
end

-- youtube: hook
--webview.init_funcs.youtube_hook = function (view, w)
--    view:add_signal("navigation-request", function (v, uri)
--        if string.find(uri, "youtube%.com/watch") then
--            luakit.spawn(string.format("luakit_youtube %q", uri))
--            return false
--        end 
--    end)
--end

webview.methods.browse_history_dmenu = function( view, w )
    local db_file = luakit.data_dir .. "/history.db"
    local query = "\"select datetime(last_visit,'unixepoch'), title, uri from history order by last_visit DESC;\""
    local filter = '"ERROR:|localhost|luakit:|Page not found|[Ss]earch"'
    local dmenu = "$(dlist) -p 'History:'"
    local fh = io.popen( "echo " .. query .. " | sqlite3 " .. db_file .. " | egrep -v " .. filter .. " | sed 's/[|]/  ::  /g' | " .. dmenu .. " | sed 's/.* :: .* :: \\(.*\\)/\\1/'", "r" )
    local selection = fh:read( "*a" )
    fh:close()
    if selection ~= "" then w:new_tab( selection ) end
end

webview.methods.browse_bookmarks_dmenu = function( view, w )
    local db_file = luakit.data_dir .. "/bookmarks.db"
    local query = "\"select title, tags, uri from bookmarks order by tags,title ASC;\""
    local dmenu = "$(dlist) -p 'Bookmark:'"
    local fh = io.popen( "echo " .. query .. " | sqlite3 " .. db_file .. " | sed 's/[|]/  ::  /g' | " .. dmenu .. " | sed 's/.* :: .* :: \\(.*\\)/\\1/'", "r" )
    local selection = fh:read( "*a" )
    fh:close()
    if selection ~= "" then w:new_tab( selection ) end
end

webview.methods.google_translate = function( view, w )
    w:navigate("http://www.google.com/translate_c?langpair=en&u=" .. view.uri)
end

-- =================================================================================
