-- Global variables for luakit
globals = {
 -- homepage                = "http://luakit.org/",
-- =================================================================================
 -- homepage                = "http://github.com/mason-larobina/luakit",
 -- homepage                = "luakit://history",
 -- homepage                = "http://www.handinhandclock.com/",
 -- homepage                = "https://github.com/baskerville/bspwm",
    homepage                = "https://www.archlinux.org/",
 -- homepage                = "https://reader.curata.com/#articles/all",
 -- homepage                = "http://ttrss/#f=-3&c=0",
    scroll_step         = 40,
    zoom_step           = 0.1,
    max_cmd_history     = 100,
    max_srch_history    = 100,
 -- proxy must now be set through proxy command; environment variable is broken
 -- http_proxy          = "http://example.com:3128",
 -- default_window_size = "800x600",
 -- default_window_size = "1024x600",
    default_window_size = "1680x1050",
    show_scrollbars = true,
    term = "x-terminal-emulator",
 -- http_proxy = "http://localhost:8118",
-- =================================================================================

 -- Disables loading of hostnames from /etc/hosts (for large host files)
 -- load_etc_hosts      = false,
 -- Disables checking if a filepath exists in search_open function
 -- check_filepath      = false,
}

-- Make useragent
local _, arch = luakit.spawn_sync("uname -m")
-- If luakit doesn't start, try replacing the above line with the output of
-- `uname -m`, such as:
-- local arch = 'x86_64'

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

default_engine = "http://www.google.com/search?nomo=1&hl=en&source=hp&q="
search_engines = {
-- =================================================================================
--  luakit      = "http://luakit.org/search/index/luakit?q=%s",
--  google      = "http://google.com/search?q=%s",
--  duckduckgo  = "http://duckduckgo.com/?q=%s&t=debian",
--  github      = "https://github.com/search?q=%s",
--  google      = "http://google.com/search?q=%s",
--  wikipedia   = "http://en.wikipedia.org/wiki/Special:Search?search=%s",
--  debbugs     = "http://bugs.debian.org/%s",
--  imdb        = "http://www.imdb.com/find?s=all&q=%s",
--  sourceforge = "http://sf.net/search/?words=%s",
--  netflix     = "http://dvd.netflix.com/Search?v1=%s",
    on          = "http://1337x.org/search/%s/0/",
    al          = "http://www.allmusic.com/search/all/%s",
    --am          = "http://www.amazon.ca/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s",
    am          = "http://www.amazon.ca/s/ref=nb_sb_noss?url=search-alias&field-keywords=%s",
    an          = "http://www.anime-planet.com/anime/all?name=%s",
    ar          = default_engine .. "archlinux %s",
    as          = "http://www.asiatorrents.me/index.php?page=torrents&options=0&active=0&search=%s",
    au          = "https://aur.archlinux.org/packages/?O=0&C=0&SeB=nd&outdated=&SB=n&SO=a&PP=50&do_Search=Go&K=%s",
    aw          = "https://wiki.archlinux.org/index.php?title=Special:Search&search=%s",
    bi          = "http://www.bing.com/search?q=%s",
    bs          = "http://www.binsearch.net?q=%s",
    bu          = "http://bugs.debian.org/%s",
    ca          = "http://www.canadacomputers.com/advanced_search_result.php?keywords=%s",
    co          = "http://www.costco.ca/CatalogSearch?storeId=10302&catalogId=11201&langId=-24&refine=&keyword=%s",
    cr          = "http://crunchbanglinux.org/forums/search/?action=search&keywords=%s&show_as=topics",
    de          = "http://www.demonoid.ph/files/?query=%s",
    di          = "http://dictionary.reference.com/browse/%s",
    du          = "http://duckduckgo.com/?q=%s&t=debian",
    eb          = "http://www.ebay.com/sch/i.html?_from=R40&_sacat=0&_sop=15&_nkw=%s&_pgn=2&_skc=50&rt=nc",
    ei          = "http://8tracks.com/explore/songs/%s",
    ge          = "https://01100111011001010110010101101011.info/search/%s?t=2000",
    gi          = "https://github.com/search?q=%s",
    go          = "http://www.google.com/search?nomo=1&hl=en&source=hp&q=%s",
    ho          = "http://www.haskell.org/hoogle/?hoogle=%s",
    im          = "http://www.imdb.com/find?s=all&q=%s",
    ja          = "http://www.jango.com/music/%s",
    ki          = "http://kat.ph/usearch/%s category:movies/",
    lu          = "http://luakit.org/search/index/luakit?q=%s",
    mr          = "http://www.mrqe.com/search?q=%s",
    ne          = "https://newztown.co.za/search/%s?t=2000",
    ni          = default_engine .. "nixos linux %s",
    nm          = "https://nmatrix.co.za/search/%s",
    np          = "http://nixos.org/nixos/packages.html",
    nw          = "https://nixos.org/w/index.php?search=%s",
    --nz          = "http://nzbindex.nl/search/?q=%s",
    nz          = "https://nzbx.co/s?q=%s",
    pa          = "https://www.archlinux.org/packages/?sort=&maintainer=&flagged=&arch=any&q=%s",
    pr          = "http://predb.me/?search=%s",
    re          = default_engine .. "review %s",
    ro          = "http://www.rottentomatoes.com/search/?search=%s",
    rs          = "http://ctrlq.org/rss/",
    ru          = default_engine .. "rumor %s",
    sd          = "http://sdothum.imgur.com/all/",
    sf          = "http://sf.net/search/?words=%s",
    so          = "http://songza.com/search/?q=%s",
    su          = "http://www.subtitleseeker.com/search/TITLES/%s",
    th          = "http://thesaurus.com/browse/%s",
    us          = "http://usenet-crawler.com/search/%s?t=2000",
    wi          = "http://en.wikipedia.org/wiki/Special:Search?search=%s",
    yo          = "http://www.youtube.com/results?search_query=%s",
    wo          = "http://www.wolframalpha.com/input/?i=%s",
-- =================================================================================
}

-- Set google as fallback search engine
-- search_engines.default = search_engines.duckduckgo
-- see above
search_engines.default = search_engines.go
-- search_engines.default = search_engines.du
-- Use this instead to disable auto-searching
--search_engines.default = "%s"

-- Per-domain webview properties
-- See http://webkitgtk.org/reference/webkitgtk/stable/WebKitWebSettings.html
domain_props = {
-- =================================================================================
    ["all"] = {
        enable_scripts          = true,
        enable_plugins          = true,
        enable_private_browsing = false,
        -- curata skin css
        user_stylesheet_uri     = "file://" .. luakit.data_dir .. "/styles/custom.css",
        monospace_font_family   = "Liberation Mono",
        sans_serif_font_family  = "Sans-Serif",
        serif_font_family       = "Sans-Serif",
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
-- =================================================================================
    ["bbs.archlinux.org"] = {
        user_stylesheet_uri     = "file://" .. luakit.data_dir .. "/styles/dark.css",
        enable_private_browsing = true,
    },
}

-- vim: et:sw=4:ts=8:sts=4:tw=80
