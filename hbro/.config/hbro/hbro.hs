{-# LANGUAGE FlexibleContexts #-}
module Main where

-- {{{ Imports
import Hbro
import qualified Hbro.Bookmarks as Bookmarks
import qualified Hbro.Clipboard as Clipboard
import qualified Hbro.Download as Download
import qualified Hbro.Gui as GUI
import qualified Hbro.History as History
import Hbro.Keys as Key
import Hbro.Misc
import Hbro.Network
import Hbro.Notification
import qualified Hbro.Prompt as Prompt
-- import Hbro.Session
import Hbro.Settings
import Hbro.StatusBar
import qualified Hbro.Webkit.WebSettings as WS

import Control.Conditional
import Control.Lens hiding((??))
import Control.Monad hiding(forM_, mapM_)

import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.WebKit.WebSettings

import Network.URI hiding(parseURI, parseURIReference)

import Prelude hiding(mapM_)

import System.Directory
import System.Environment.XDG.BaseDir
import System.FilePath
-- }}}


myHomePage = URI "http:" (Just $ URIAuth "" "//www.google.com" "") "" "" "" -- Seriously ?

-- Download to $HOME
myDownloadHook :: URI -> String -> Int -> K ()
myDownloadHook uri filename _size = do
    destination <- io getHomeDirectory
    Download.aria destination uri filename

myLoadFinishedHook :: K ()
myLoadFinishedHook = History.log =<< getUserDataDir "hbro" >/> "history"

-- Setup (run at start-up)
-- Note that keybindings are suited for an azerty keyboard.
mySetup :: K ()
mySetup = do
    myHistoryFile   <- getUserDataDir "hbro" >/> "history"
    myBookmarksFile <- getUserDataDir "hbro" >/> "bookmarks"

-- Browse
    bind Key.Normal "C-<Left>"  $     goBackList    ["-l", "10"] >>= load
    bind Key.Normal "C-<Right>" $     goForwardList ["-l", "10"] >>= load
    bind Key.Normal "C-g"       $     Prompt.read "DuckDuckGo search" "" (load <=< parseURIReference . ("http://duckduckgo.com/html?q=" ++) . escapeURIString isAllowedInURI)
-- Bookmarks
    bind Key.Normal "C-d"       $     Prompt.read "Bookmark with tags:" "" $ Bookmarks.add myBookmarksFile . words
{-    bind Key.Normal "C-D"       $     Prompt.read "Bookmark all instances with tag:" "" $ \tags -> do
        uris <- mapM parseURI =<< sendCommandToAll "GET_URI"
        forM uris $ Bookmarks.addCustom myBookmarksFile . (`Bookmarks.Entry` words tags)
        void . Bookmarks.addCustom myBookmarksFile . (`Bookmarks.Entry` words tags) =<< getURI-}
    bind Key.Normal "M-d"       $     Bookmarks.deleteWithTag myBookmarksFile ["-l", "10"]
    bind Key.Normal "C-l"       $     Bookmarks.select        myBookmarksFile ["-l", "10"] >>= load
    bind Key.Normal "C-L"       $     Bookmarks.selectTag     myBookmarksFile ["-l", "10"] >>= void . mapM (\uri -> io $ spawn "hbro" ["-u", (show uri)])
--    ("C-q"),           webViewGetUri webView >>= maybe (return ()) (Queue.append),
--    ("M-q"),           \b -> do
--        uri <- Queue.popFront
--        load uri b),

-- History
    bind Key.Normal "C-h"       $     History.select myHistoryFile ["-l", "10"] >>= load . History.mURI
-- Session
    --("M-l"),           loadFromSession ["-l", "10"])
-- Settings
    bind Key.Normal "M-j"       $     WS.toggle webSettingsEnableScripts >>= ((notify 5000 "Javascript disabled") ?? (notify 5000 "Javascript enabled"))
    bind Key.Normal "M-p"       $     WS.toggle webSettingsEnablePlugins >>= ((notify 5000 "Plugins disabled") ?? (notify 5000 "Plugins enabled"))

-- Web settings (cf Graphic.Gtk.WebKit.WebSettings)
    WS.modify webSettingsEnablePlugins                     $ const False
    WS.modify webSettingsEnableScripts                     $ const False
    WS.modify webSettingsEnablePageCache                   $ const True
    WS.modify webSettingsJSCanOpenWindowAuto               $ const True
    WS.modify webSettingsUserAgent                         $ const firefoxUserAgent

-- Scroll position in status bar
    setupScrollWidget =<< GUI.getObject castToLabel "scroll"

-- Zoom level in status bar
    setupZoomWidget =<< GUI.getObject castToLabel "zoom"

-- Load progress in status bar
    setupProgressWidget =<< GUI.getObject castToLabel "progress"

-- Current URI in status bar
    setupURIWidget defaultURIColors defaultSecureURIColors =<< GUI.getObject castToLabel "uri"

-- Keystrokes in status bar
    setupKeyStrokesWidget =<< GUI.getObject castToLabel "keys"

-- Session manager
    --setupSession browser

-- Favicon
    --_ <- on webView iconLoaded $ \uri -> do something

    return ()


myConfig :: Config K -> Config K
myConfig = id
    . set homePage       myHomePage
    . set onDownload     myDownloadHook
    . set onLoadFinished myLoadFinishedHook


-- Main function, expected to call 'hbro'
main :: IO ()
main = hbro mySetup
