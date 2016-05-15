-- =================================================================================
-- setting to enable reasonably long cookie persistence
cookies.session_timeout = 60*60*24*365
cookies.force_session_timeout = false
cookies.store_session_cookies = true

-- my keymap extensions
local key, buf, but = lousy.bind.key, lousy.bind.buf, lousy.bind.but

add_binds("normal", {

    key({}, ";", "Enter `command` mode.",
        function (w) w:set_mode("command") end),

    key({}, "b", "Go back in the browser history.",
        function (w, m) w:back(m.count) end),

    key({"Control"}, "P", "Print current page.",
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

    key({}, "F11", "Toggle fullscreen mode.",
        -- see function window.init_funcs.hide_tablist_fullscreen (in rc.lua)
        function (w)
            w.win.fullscreen = not w.win.fullscreen
            if w.win.fullscreen then
                if my_show_tablist then w.tablist.widget:show() end
            else
                w.tablist.widget:hide()
            end
        end),

    key({"Control"}, "F11", "Toggle tablist mode.",
        -- see function window.init_funcs.hide_tablist_fullscreen (in rc.lua)
        function (w)
            my_show_tablist = not my_show_tablist
            if my_show_tablist then
                w.tablist.widget:show()
            else
                w.tablist.widget:hide()
            end
        end),

    buf("^gA$", "Open nzbmatrix audio.",
        function (w) w:navigate("http://nzbmatrix.com/nzb-search.php?cat=23") end),

    buf("^ga$", "Open nzbmatrix anime.",
        function (w) w:navigate("http://nzbmatrix.com/nzb-search.php?search=720p&cat=28") end),

    buf("^gM$", "Open nzbmatrix tv.",
        function (w) w:navigate("http://nzbmatrix.com/nzb-search.php?search=720p&cat=tv-all") end),

    buf("^gm$", "Open nzbmatrix movies.",
        function (w) w:navigate("http://nzbmatrix.com/nzb-search.php?search=720p&cat=movies-all") end),

    buf("^gW$", "Open thedarnedestthing.com.",
        function (w, c) w:enter_cmd(":tabopen http://thedarnedestthing.com/admin/do_") end),

    buf("^gw$", "Open thedarnedestthing.",
        function (w) w:navigate("http://thedarnedestthing.com/") end),

    key({}, "F8", "Open localhost://thedarnedestthing/",
        function (w) w:navigate("http://thedarnedestthing/") end),

    key({"Control"}, "F8", "Admin localhost://thedarnedestthing/",
        function (w, c) w:enter_cmd(":tabopen http://thedarnedestthing/admin/do_") end),

    key({"Shift"}, "F8", "List thedarnedestthing orphan articles.",
        function (w) w:navigate("http://thedarnedestthing/admin/do_orphan") end),

    key({"Shift", "Control"}, "F8", "Process thedarnedestthing email comments.",
        function (w) w:navigate("http://thedarnedestthing/admin/do_mail") end),

    key({}, "F4", "My readability script.",
        function (w)
            w.view:eval_js("(function(){_readableOptions={'text_font':'Helvetica,%20quote(Helvetica%20Neuve),%20Arial,%20Tahoma,%20sans-serif','text_font_monospace':'quote(Latin%20Modern%20Mono%20Caps)','text_font_header':'Helvetica','text_size':'13px','text_line_height':'1.5','box_width':'36em','color_text':'#BBBBBB','color_background':'#293235','color_links':'#00EEFF','text_align':'normal','base':'web_readability','custom_css':''};if(document.getElementsByTagName('body').length>0);else{return;}if(window.$readable){if(window.$readable.bookmarkletTimer){return;}}else{window.$readable={};}window.$readable.bookmarkletTimer=true;window.$readable.options=_readableOptions;if(window.$readable.bookmarkletClicked){window.$readable.bookmarkletClicked();return;}_readableScript=document.createElement('script');_readableScript.setAttribute('src','http://readable-static.tastefulwords.com/target.js?rand='+encodeURIComponent(Math.random()));document.getElementsByTagName('body')[0].appendChild(_readableScript);})()")
            -- w:set_mode("passthrough")
            w:set_mode("normal")
        end),

    key({"Shift","Control"}, "M", "Add to my delicious account.",
        function (w)
            -- w:set_mode("passthrough")
            w:set_mode("insert")
            w.view:eval_js("(function(){location.href='http://del.icio.us/post?v=3&url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title);})()")
        end),

    key({"Shift"}, "Return", "Blocking external editor.",
        function (w)
            local s = w:eval_js("document.activeElement.value")
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
            luakit.spawn_sync("/usr/bin/terminator -e '/usr/bin/vim -g -f -c \"set formatoptions=twan1 textwidth=72\" -c \"set nocp spell wrap\" " .. n .. " '")

            f = io.open(n, "r")
            s = f:read("*all")
            f:close()
            -- Strip the string
            s = s:gsub("^%s*(.-)%s*$", "%1")
            -- Escape it but remove the quotes
            s = string.format("%q", s):sub(2, -2)
            -- lua escaped newlines (slash+newline) into js newlines (slash+n)
            s = s:gsub("\\\n", "\\n")
            w:eval_js("document.activeElement.value = '" .. s .. "'")
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

                w:eval_js(string.format([=[
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

            local s = w:eval_js(string.format([=[
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
    view.show_scrollbars = true 
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
            local cmd = string.format("%s \"%q\"", "xterm -T mailto -e mutt", uri)
            luakit.spawn(cmd)
            return false
        end
    end)
end

-- downloads.default_dir = os.getenv("HOME") .. "/down"
downloads.default_dir = "/net/downloads/http"

-- save last session
local close_win = window.methods.close_win
window.methods.close_win = function (w, ...)
    session.save{w}
    close_win(w, ...)
end
-- =================================================================================
