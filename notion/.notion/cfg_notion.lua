--
-- Notion main configuration file
--
-- This file only includes some settings that are rather frequently altered.
-- The rest of the settings are in cfg_notioncore.lua and individual modules'
-- configuration files (cfg_modulename.lua).
--
-- When any binding and other customisations that you want are minor, it is
-- recommended that you include them in a copy of this file in ~/.notion/.
-- Simply create or copy the relevant settings at the end of this file (from
-- the other files), recalling that a key can be unbound by passing 'nil'
-- (without the quotes) as the callback. For more information, please see
-- the Notion configuration manual available from the Notion Web page.
--

-- Set default modifiers. Alt should usually be mapped to Mod1 on
-- XFree86-based systems. The flying window keys are probably Mod3
-- or Mod4; see the output of 'xmodmap'.
-- These may be defined in /etc/default/notion, loaded as cfg_debian.
dopath("cfg_debian")
--META="Mod1+"
--ALTMETA=""
--META="Mod4+"

-- Terminal emulator
-- cannot assign terminator as it breaks F1 help / man pages, assign xfce4-terminal to x-terminal-emulator instead
--XTERM="xterm"
XTERM="terminator"

function file_exists(name)
  local f = io.open(name, "r")
  if f ~= nil then io.close(f) return true else return false end
end

require ("socket")
multihead = socket.dns.gethostname() == "luna" and true or false
netbook = (not multihead)

-- Some basic settings
ioncore.set {
  -- Maximum delay between clicks in milliseconds to be considered a
  -- double click.
  --dblclick_delay=250,

  -- For keyboard resize, time (in milliseconds) to wait after latest
  -- key press before automatically leaving resize mode (and doing
  -- the resize in case of non-opaque move).
  --kbresize_delay=1500,

  -- Opaque resize?
  --opaque_resize=false,

  -- Movement commands warp the pointer to frames instead of just
  -- changing focus. Enabled by default.
  --warp=true,

  -- Switch frames to display newly mapped windows
  --switchto=true,

  -- Default index for windows in frames: one of 'last', 'next' (for
  -- after current), or 'next-act' (for after current and anything with
  -- activity right after it).
  --frame_default_index='next',

  -- Auto-unsqueeze transients/menus/queries.
  --unsqueeze=true,

  -- Display notification tooltips for activity on hidden workspace.
  --screen_notify=true,
}

-- Load default settings. The file cfg_defaults loads all the files
-- commented out below, except mod_dock. If you do not want to load
-- something, comment out this line, and uncomment the lines corresponding
-- the the modules or configuration files that you want, below.
-- The modules' configuration files correspond to the names of the
-- modules with 'mod' replaced by 'cfg'.
--dopath("cfg_defaults")

-- Load configuration of the Notion 'core'. Most bindings are here.
dopath("cfg_notioncore")

-- Load some kludges to make apps behave better.
dopath("cfg_kludges")

-- Define some layouts.
dopath("cfg_layouts")

-- Load some modules. Bindings and other configuration specific to modules
-- are in the files cfg_modulename.lua.
dopath("mod_query")
dopath("mod_menu")
dopath("mod_tiling")
--dopath("mod_statusbar")
if file_exists("/home/shum/.statusbar") then dopath("mod_statusbar") end
--dopath("mod_dock")
dopath("mod_sp")
dopath("mod_xinerama")
dopath("mod_xrandr")

--
-- Common customisations
--

-- Uncommenting the following lines should get you plain-old-menus instead
-- of query-menus.

--defbindings("WScreen", {
--    kpress(ALTMETA.."F12", "mod_menu.menu(_, _sub, 'mainmenu', {big=true})"),
--})
--
--defbindings("WMPlex.toplevel", {
--    kpress(META.."M", "mod_menu.menu(_, _sub, 'ctxmenu')"),
--})

-- refresh xinerama on screen layout updates
function screenlayoutupdated()
  mod_xinerama.refresh()
end

randr_screen_change_notify_hook = ioncore.get_hook('randr_screen_change_notify')

if randr_screen_change_notify_hook ~= nil then
  randr_screen_change_notify_hook:add(screenlayoutupdated)
end

-- frame assignments
defwinprop {
  class = "Audacious",
  role = "audacious",
  target = "Music",
}

defwinprop {
  class = "Binreader",
  role = "binreader",
  target = "NZB",
}

defwinprop {
  class = "Conky",
  instance = "Conky",
  target = "Conky",
}

defwinprop {
  class = "ConkyImage",
  instance = "ConkyImage",
  target = "ConkyImage",
}

defwinprop {
  class = "Defaut-editor",
  instance = "defaut-editor",
  target = "Document",
}

defwinprop {
  class = "Emesene",
  instance = "emesene",
  target = "Chat",
}

defwinprop {
  class = "Googlechrome",
  instance = "googlechrome",
  target = "WWW",
}

defwinprop {
  class = "Gvim",
  instance = "gvim",
  target = "Writeroom",
}

defwinprop {
  class = "Ghb",
  instance = "ghb",
  target = "Video",
}

defwinprop {
  class = "Ktouch",
  instance = "ktouch",
  target = "WWW",
}

defwinprop {
  class = "Last.fm",
  instance = "last.fm",
  target = "Last.fm",
}

defwinprop {
  class = "Launchy",
  instance = "launchy",
  float = true,
  transient_mode = "off",
  transparent = true,
  userpos = true,
}

defwinprop {
  class = "luakit",
  instance = "luakit",
  target = "WWW",
}

defwinprop {
  class = "Minitube",
  instance = "minitube",
  target = "WWW",
}

defwinprop {
  class = "Minitube.original",
  instance = "minitube.original",
  target = "WWW",
}

defwinprop {
  class = "Pavucontrol",
  instance = "pavucontrol",
  target = "Audio",
}

defwinprop {
  class = "Pidgin",
  instance = "Pidgin",
  target = "Chat",
}

defwinprop {
  class = "Smplayer.original",
  instance = "smplayer.original",
  target = "Video",
}

-- stalonetray system tray (see cfg_statusbar)
defwinprop {
  class = "stalonetray",
  statusbar = "systray"
}

defwinprop {
  class = "Streamtuner2",
  instance = "streamtuner2",
  target = "Music",
}

defwinprop {
  class = "Sublime-text",
  instance = "sublime-text",
  target = "Document",
}

--defwinprop {
--  class = "Synapse",
--  instance = "synapse",
--  float = true,
--  transient_mode = "off",
--  userpos = true,
--}

--defwinprop {
--  class = "Thunar",
--  instance = "thunar",
--  target = "Document",
--}

defwinprop {
  class = "Tixati",
  instance = "tixati",
  target = "BitTorrent",
}

defwinprop {
  class = "Transmission-gtk",
  instance = "transmission-gtk",
  target = "BitTorrent",
}

defwinprop {
  class = "trayion",
  statusbar = "systray"
}

defwinprop {
  class = "Umplayer.original",
  instance = "umplayer.original",
  target = "Video",
}

--defwinprop {
--  class = "VirtualBox",
--  instance = "Qt-subapplication",
--  target = "Video",
--}

defwinprop {
  class = "Vlc",
  instance = "vlc",
  target = "Video",
}

defwinprop {
  class = "xbmc.bin",
  instance = "xbmc.bin",
  target = "Video",
}

defwinprop {
  class = "Zathura",
  instance = "zathura",
  target = "Document",
}

-- cli specific apps launched with xfce4-terminal
-- using --role of xfce4-terminal (vs -name of xterm and notion "instance") to
-- identify application
-- probably can omit "class" specification but useful for self-documenting

--defwinprop {
--  class = "Xfce4-terminal",
--  role = "bittorrent",
--  target = "BitTorrent",
--}

defwinprop {
  class = "Xfce4-terminal",
  role = "chat",
  target = "Chat",
}

defwinprop {
  class = "Xfce4-terminal",
  role = "document",
  target = "Document",
}

defwinprop {
  class = "Xfce4-terminal",
  role = "imap",
  target = "IMAP",
}

defwinprop {
  class = "Xfce4-terminal",
  role = "mail",
  target = "Mail",
}

defwinprop {
  class = "Xfce4-terminal",
  role = "music",
  target = "Music",
}

defwinprop {
  class = "Xfce4-terminal",
  role = "nzb",
  target = "NZB",
}

defwinprop {
  class = "Xfce4-terminal",
  role = "process",
  target = "Process",
}

defwinprop {
  class = "Xfce4-terminal",
  role = "radio",
  target = "Radio",
}

defwinprop {
  class = "Xfce4-terminal",
  role = "rss",
  target = multihead and "RSS" or "WWW",
}

defwinprop {
  class = "Xfce4-terminal",
  role = "shell",
  target = "Shell",
}

defwinprop {
  class = "Xfce4-terminal",
  role = "syslog",
  target = "Syslog",
}

defwinprop {
  class = "Xfce4-terminal",
  role = "twitter",
  target = "Twitter",
}

defwinprop {
  class = "Xfce4-terminal",
  role = "usenet",
  target = "Usenet",
}

defwinprop {
  class = "Xfce4-terminal",
  role = "video",
  target = "Video",
}

-- floating windows

defwinprop {
  class = "Gimp",
  float = true,
}

-- window manager session startup
os.execute("~/bin/autostart")
