-- | Designed to be imported as @qualified@.
module Hbro.History (
    Entry(..),
    log,
    add,
    parseEntry,
    select
) where

-- {{{ Imports
import Hbro hiding(log)
-- import Hbro.Error
import Hbro.Gui
import Hbro.Misc
import Hbro.Network

import Control.Exception
import Control.Monad.Base
import Control.Monad.Error

import Data.Functor
import Data.List
import Data.Time

import Network.URI (URI)

import Prelude hiding(log)

--import System.IO.Error
import System.IO
import System.Locale
-- }}}

-- {{{ Type definitions
data Entry = Entry {
    mTime  :: LocalTime,
    mURI   :: URI,
    mTitle :: String
}

instance Show Entry where
    show (Entry time uri title) = unwords [(formatTime defaultTimeLocale dateFormat time), show uri, title]

dateFormat :: String
dateFormat = "%F %T"
-- }}}

-- | Log current visited page to history file
log :: (MonadBase IO m, ConfigReader n m, GUIReader n m, MonadError HError m) => FilePath -> m ()
log file = do
    uri      <- getURI
    title    <- getTitle
    timeZone <- io $ utcToLocalTime <$> getCurrentTimeZone
    now      <- io $ timeZone <$> getCurrentTime

    add file (Entry now uri title)

-- | Add a new entry to history file
add :: (MonadBase IO m, ConfigReader n m, MonadError HError m)
    => FilePath     -- ^ History file
    -> Entry        -- ^ History entry to add
    -> m ()
add file newEntry = do
    logV $ "Adding new history entry <" ++ show (mURI newEntry) ++ ">"
    either (throwError . IOE) return =<< (io . try $ withFile file AppendMode (`hPutStrLn` show newEntry))
    --either (\e -> errorHandler file' e >> return False) (const $ return True) result

-- | Try to parse a String into a history Entry.
parseEntry :: (MonadError HError m) => String -> m Entry
parseEntry [] = throwError $ OtherError "While parsing history entry: empty input."
parseEntry line = (parseEntry' . words) line

parseEntry' :: (MonadError HError m) => [String] -> m Entry
parseEntry' (d:t:u:t') = do
    time <- maybe (throwError $ OtherError "While parsing history entry: invalid date.") return $ parseTime defaultTimeLocale dateFormat (unwords [d, t])
    uri  <- parseURI u

    return $ Entry time uri (unwords t')
parseEntry' _ = throwError $ OtherError "While parsing history entry: invalid format."

-- | Open a dmenu with all (sorted alphabetically) history entries, and return the user's selection, if any
select :: (Functor m, MonadBase IO m, MonadError HError m)
       => FilePath          -- ^ Path to history file
       -> [String]          -- ^ dmenu's commandline options
       -> m Entry           -- ^ Selected history entry, if any
select file dmenuOptions = do
    --either (\e -> errorHandler file' e >> return Nothing) (return . return) result
    parseEntry =<< dmenu dmenuOptions . unlines . reverse . sort . nub . lines =<< either (throwError . IOE) return =<< (io . try $ readFile file)
