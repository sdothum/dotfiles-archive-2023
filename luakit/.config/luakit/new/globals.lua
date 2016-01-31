-- Global variables for luakit
globals = {
 -- homepage                = "http://luakit.org/",
-- =================================================================================
 -- homepage                = "http://github.com/mason-larobina/luakit",
    homepage                = "luakit://history",
    scroll_step         = 40,
    zoom_step           = 0.1,
    max_cmd_history     = 100,
    max_srch_history    = 100,
 -- http_proxy          = "http://example.com:3128",
 -- default_window_size = "800x600",
 -- proxy must now be set through proxy command; environment variable is broken
 -- default_window_size = "1024x600",
    default_window_size = "1680x1050",
    show_scrollbars = true,
    term = "x-terminal-emulator",
    http_proxy = "http://localhost:8118",
-- =================================================================================

 -- Disables loading of hostnames from /etc/hosts (for large host files)
 -- load_etc_hosts      = false,
 -- Disables checking if a filepath exists in search_open function
 -- check_filepath      = false,
}

-- Make useragent
local _, arch = luakit.spawn_sync("uname -sm")
-- Only use the luakit version if in date format (reduces identifiability)
local lkv = string.match(luakit.version, "^(%d+.%d+.%d+)")
globals.useragent = string.format("Mozilla/5.0 (%s) AppleWebKit/%s+ (KHTML, like Gecko) WebKitGTK+/%s luakit%s",
    string.sub(arch, 1, -2), luakit.webkit_user_agent_version,
    luakit.webkit_version, (lkv and ("/" .. lkv)) or "")

-- Search common locations for a ca file which is used for ssl connection validation.
local ca_files = {
    -- $XDG_DATA_HOME/luakit/ca-certificates.crt
    luakit.data_dir .. "/ca-certificates.crt",
    "/etc/certs/ca-certificates.crt",
    "/etc/ssl/certs/ca-certificates.crt",
}
-- Use the first ca-file found
for _, ca_file in ipairs(ca_files) do
    if os.exists(ca_file) then
        soup.ssl_ca_file = ca_file
        break
    end
end

-- Change to stop navigation sites with invalid or expired ssl certificates
soup.ssl_strict = false

-- Set cookie acceptance policy
cookie_policy = { always = 0, never = 1, no_third_party = 2 }
soup.accept_policy = cookie_policy.always

-- List of search engines. Each item must contain a single %s which is
-- replaced by URI encoded search terms. All other occurances of the percent
-- character (%) may need to be escaped by placing another % before or after
-- it to avoid collisions with lua's string.format characters.
-- See: http://www.lua.org/manual/5.1/manual.html#pdf-string.format
search_engines = {
-- =================================================================================
--  luakit      = "http://luakit.org/search/index/luakit?q=%s",
--  google      = "http://google.com/search?q=%s",
--  duckduckgo  = "http://duckduckgo.com/?q=%s",
--  wikipedia   = "http://en.wikipedia.org/wiki/Special:Search?search=%s",
--  debbugs     = "http://bugs.debian.org/%s",
--  imdb        = "http://imdb.com/find?s=all&q=%s",
--  sourceforge = "http://sf.net/search/?words=%s",
--  bi          = "http://www.bing.com/search?q=%s",
    al          = "http://www.allmusic.com/search/all/%s",
    an          = "http://www.anime-planet.com/anime/all?name=%s",
    bi          = "http://www.binsearch.net?q=%s+720p",
    cr          = "http://crunchbanglinux.org/forums/search/?action=search&keywords=%s&show_as=topics",
    de          = "http://www.delicious.com/search?p=%s&user=shum",
    di          = "http://dictionary.reference.com/browse/%s",
    du          = "http://duckduckgo.com/?q=%s",
    go          = "http://google.com/search?q=%s",
    im          = "http://imdb.com/find?s=all&q=%s",
    lu          = "http://luakit.org/search/index/luakit?q=%s",
    mr          = "http://www.mrqe.com/search?q=%s",
    nz          = "http://nzbmatrix.com/nzb-search.php?search=%s+720p",
    ro          = "http://www.rottentomatoes.com/search/?search=%s",
    sf          = "http://sf.net/search/?words=%s",
    su          = "http://www.subtitleseeker.com/search/TITLES/%s",
    th          = "http://thesaurus.com/browse/%s",
    wi          = "http://en.wikipedia.org/wiki/Special:Search?search=%s",
    yo          = "http://www.youtube.com/results?search_query=%s",
    wo          = "http://www.wolframalpha.com/input/?i=%s",
-- =================================================================================
}

-- Set google as fallback search engine
-- search_engines.default = search_engines.google
-- see above
-- search_engines.default = search_engines.bi
search_engines.default = search_engines.go
-- Use this instead to disable auto-searching
--search_engines.default = "%s"

-- Per-domain webview properties
-- See http://webkitgtk.org/reference/webkitgtk-WebKitWebSettings.html
domain_props = { [[
-- =================================================================================
    ["all"] = {
        enable_scripts          = true,
        enable_plugins          = true,
        enable_private_browsing = false,
        user_stylesheet_uri     = "file://" .. luakit.data_dir .. "/styles/custom.css",
    },
    ["cbc.ca"] = {
        enable_scripts = true,
        enable_plugins = true,
    },
    ["google.com"] = {
        enable_scripts = true,
        enable_plugins = true,
    },
    ["google.ca"] = {
        enable_scripts = true,
        enable_plugins = true,
    },
    ["youtube.com"] = {
        enable_scripts = true,
        enable_plugins = true,
    },
    ["bbs.archlinux.org"] = {
        user_stylesheet_uri     = "file://" .. luakit.data_dir .. "/styles/dark.css",
        enable_private_browsing = true,
    }, ]]
-- =================================================================================
}

-- vim: et:sw=4:ts=8:sts=4:tw=80
