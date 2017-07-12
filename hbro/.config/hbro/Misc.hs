module Hbro.Misc where

-- {{{ Imports
import Hbro
-- import Hbro.Error
import Hbro.Gui
import Hbro.Network

import Control.Exception
import Control.Monad.Base
import Control.Monad.Error

-- import Data.Functor
import Data.Maybe

-- import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.WebKit.WebBackForwardList
import Graphics.UI.Gtk.WebKit.WebHistoryItem
import Graphics.UI.Gtk.WebKit.WebView

import Network.URI (URI)

import System.IO
import System.Process
-- }}}


-- | Open dmenu with given input and return selected entry.
dmenu :: (Functor m, MonadBase IO m, MonadError HError m)
      => [String]    -- ^ dmenu's commandline options
      -> String      -- ^ dmenu's input
      -> m String    -- ^ Selected entry
dmenu options input = do
    (in_, out, err, pid) <- io $ runInteractiveProcess "dmenu" options Nothing Nothing
    io $ hPutStr in_ input
    io $ hClose in_

    output <- either (throwError . IOE) return =<< (io . try $ hGetLine out)

    io (hClose out) >> io (hClose err) >> (void . io $ waitForProcess pid)
    return output


-- | List preceding URIs in dmenu and let the user select which one to load.
goBackList :: (Functor m, MonadBase IO m, GUIReader n m, MonadError HError m) => [String] -> m URI
goBackList dmenuOptions = do
    list           <- io . webViewGetBackForwardList =<< readGUI webView
    n              <- io $ webBackForwardListGetBackLength list
    backList       <- io $ webBackForwardListGetBackListWithLimit list n
    dmenuList      <- io $ mapM itemToEntry backList

    parseURIReference . head . words =<< (dmenu dmenuOptions . unlines . catMaybes) dmenuList


-- | List succeeding URIs in dmenu and let the user select which one to load.
goForwardList :: (Functor m, MonadBase IO m, GUIReader n m, MonadError HError m) => [String] -> m URI
goForwardList dmenuOptions = do
    list        <- io . webViewGetBackForwardList =<< readGUI webView
    n           <- io $ webBackForwardListGetForwardLength list
    forwardList <- io $ webBackForwardListGetForwardListWithLimit list n
    dmenuList   <- io $ mapM itemToEntry forwardList

    parseURIReference . head . words =<< (dmenu dmenuOptions . unlines . catMaybes) dmenuList


itemToEntry :: WebHistoryItem -> IO (Maybe String)
itemToEntry item = do
    title <- webHistoryItemGetTitle item
    uri   <- webHistoryItemGetUri   item
    case uri of
        Just u -> return $ Just (u ++ " | " ++ (maybe "Untitled" id title))
        _      -> return Nothing
