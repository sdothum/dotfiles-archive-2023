module Hbro.Settings where

-- {{{ Import
import Graphics.UI.Gtk.WebKit.WebSettings

import System.Glib.Attributes
-- }}}

-- | Disable HTML5 database & local storage, plugins and scripts.
paranoidWebSettings :: [AttrOp WebSettings]
paranoidWebSettings = [
    --webSettingsEnablePrivateBrowsing		:= False, --  Experimental
-- Privacy
    webSettingsEnableHtml5Database              := False,
    webSettingsEnableHtml5LocalStorage	        := False,
    webSettingsEnableOfflineWebApplicationCache := False,
    webSettingsEnableSiteSpecificQuirks	        := False,
    webSettingsUserAgent                        := firefoxUserAgent,
-- Security
    webSettingsEnablePlugins                    := False,
    webSettingsEnableScripts                    := False,
    webSettingsJSCanOpenWindowAuto              := False]

-- {{{ User agents
chromeUserAgent, epiphanyUserAgent, firefoxUserAgent, internetExplorerUserAgent, operaUserAgent, safariUserAgent :: String
chromeUserAgent           = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.12 Safari/535.11"
epiphanyUserAgent         = "Mozilla/5.0 (X11; U; Linux x86_64; en-US) AppleWebKit/534.7 (KHTML, like Gecko) Epiphany/2.30.6 Safari/534.7"
firefoxUserAgent          = "Mozilla/5.0 (X11; Linux i686; rv:2.0.1) Gecko/20100101 Firefox/4.0.1"
internetExplorerUserAgent = "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/6.0)"
operaUserAgent            = "Opera/9.80 (X11; Linux x86_64; U; en) Presto/2.9.168 Version/11.50"
safariUserAgent           = "Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/533.20.25 (KHTML, like Gecko) Version/5.0.4 Safari/533.20.27"
-- }}}
