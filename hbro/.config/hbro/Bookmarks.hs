-- | Designed to be imported as @qualified@.
module Hbro.Bookmarks (
    Entry(..),
    add,
    addCustom,
    select,
    selectTag,
    deleteWithTag
) where

-- {{{ Imports
import Hbro
-- import Hbro.Error
import Hbro.Gui
import Hbro.Misc
import Hbro.Network

import Control.Exception
import Control.Monad hiding(forM_, mapM_)
import Control.Monad.Base
import Control.Monad.Error hiding(forM_, mapM_)

--import qualified Data.ByteString.Char8 as B
-- import Data.Foldable hiding(find, foldr)
import Data.Functor
import Data.List
import Data.Maybe
-- import Data.Random.Extras
-- import Data.Random.RVar
-- import Data.Random.Source.DevRandom


import Network.URI (URI)

import Prelude hiding(mapM_)

import System.IO
-- }}}

-- {{{ Type definitions
data Entry = Entry {
    mURI  :: URI,
    mTags :: [String]
}

instance Show Entry where
    show (Entry uri tags) = unwords $ (show uri):tags
-- }}}

-- | Try to parse a String into a bookmark Entry.
parseEntry :: (MonadError HError m) => String -> m Entry
parseEntry [] = throwError $ OtherError "While parsing bookmarks: empty entry."
parseEntry line = return (words line)
    >>= (\(h:t) -> parseURI h
    >>= (\uri -> return $ Entry uri t))

-- | Check if the given bookmark Entry is tagged with the given tag.
hasTag :: String -> Entry -> Bool
hasTag tag = isJust . (find $ (==) tag) . mTags

-- | Add current webpage to bookmarks with given tags
add :: (Functor m, MonadBase IO m, GUIReader n m, MonadError HError m) => FilePath -> [String] -> m ()
add file tags = do
    uri <- getURI
    void . addCustom file $ Entry uri tags

-- | Add a custom entry to bookmarks
addCustom :: (MonadBase IO m, MonadError HError m)
          => FilePath      -- ^ Bookmarks' database file
          -> Entry         -- ^ New bookmarks entry
          -> m ()
addCustom file newEntry = do
    either (throwError . IOE) return =<< (io . try $ withFile file AppendMode (`hPutStrLn` show newEntry))
    --either (\e -> errorHandler file' e >> return False) (const $ return True) result

-- | Open a dmenu with all (sorted alphabetically) bookmarks entries, and return the user's selection, if any.
select :: (Functor m, MonadBase IO m, MonadError HError m)
       => FilePath         -- ^ Bookmarks' database file
       -> [String]         -- ^ dmenu's commandline options
       -> m URI
select file dmenuOptions = do
    result <- either (throwError . IOE) return =<< (io . try $ readFile file)

    --either (\e -> errorHandler file' e >> return Nothing) (\x -> return $ Just x) result
    parseURIReference . last . words =<< (dmenu dmenuOptions . unlines . sort . nub . (map reformat) . lines $ result)

reformat :: String -> String
reformat line = unwords $ tags' ++ [uri]
  where
    uri:tags = words line
    tags'    = sort $ map (\tag -> '[':(tag ++ "]")) tags

-- | Open a dmenu with all (sorted alphabetically) bookmarks tags, and return the user's selection, if any.
selectTag :: (Functor m, MonadBase IO m, MonadError HError m)
          => FilePath          -- ^ Bookmarks' database file
          -> [String]          -- ^ dmenu's commandline options
          -> m [URI]
selectTag file dmenuOptions = do
-- Read bookmarks file
    result <- either (throwError . IOE) return =<< (io . try $ readFile file)
    --file'' <- either (\e -> errorHandler file' e >> return Nothing) (\x -> return $ Just x) result

    entries <- mapM parseEntry . lines $ result
    let tags = unlines . sort . nub . words . unwords . foldr (union . mTags) [] $ entries

-- Let user select a tag
    (map mURI) . (\t -> filter (hasTag t) entries) <$> dmenu dmenuOptions tags

--
--popOldest :: PortableFilePath -> String -> IO (Maybe URI)
--popOldest file tags = do

-- Return a random Bookmark entry with a given tag, while removing it from bookmarks.
-- popRandom :: PortableFilePath
--           -> String
--           -> IO (Maybe URI)
-- popRandom file tags = do
--     file'  <- resolve file
--     result <- try . readFile $ file'
--     file'' <- either (\e -> errorHandler file' e >> return Nothing) (\x -> return $ Just x) result

--     forM_ file'' $ \f -> do
--         let selection = choiceExtract . lines $ f
--         forM_ selection $ \s -> do
--             (newLines, value) <- runRVar s DevURandom

--             renameFile file' (file' ++ ".old")
--             writeFile file' . unlines . nub $ newLines

--             return . parseURIReference . last . words $ value


-- | Remove all bookmarks entries matching the given tag.
deleteWithTag :: (Functor m, MonadBase IO m, MonadError HError m)
              => FilePath          -- ^ Bookmarks' database file
              -> [String]          -- ^ dmenu's commandline options
              -> m ()
deleteWithTag file dmenuOptions = do
    result <- either (throwError . IOE) return =<< (io . try $ readFile file)
    --file''   <- either (\e -> errorHandler file' e >> return Nothing) (\x -> return $ Just x) result

    entries <- mapM parseEntry . lines $ result
    let tags = (unlines . sort . nub . words . unwords . (foldr (union . mTags) [])) entries

    tag <- dmenu dmenuOptions tags
    io $ writeFile (file ++ ".old") $ unlines (map show entries)
    io $ writeFile file $ (unlines . (map show) . (filter (not . (hasTag tag)))) entries
