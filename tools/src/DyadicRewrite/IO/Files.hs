-- | Helper functions to interace with files.

module DyadicRewrite.IO.Files where

import System.Directory

-----------------------------------------------------------------------------------------
-- * Existence Checking.

-- | Consumes a list of filenames. Returns the first filename for which a file does not
-- exist. If all files exist, then Nothing is returned.
doFilesExist :: [String] -> IO (Maybe String)
doFilesExist []                   = return Nothing
doFilesExist (filename:filenames) = do
    exists <- doesFileExist filename
    if exists
    then doFilesExist filenames
    else return (Just filename)
