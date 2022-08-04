-- | Helper functions to interace with files.

module LafontExe.IO.Files where

import System.Directory

-----------------------------------------------------------------------------------------
-- * Reading/Writing.

-- | Pairs together the name of a file, and its contents.
data FileData = FileData String [String]

-- | Consumes the name of a file. If the file is readable, then returns its FileData.
readNamedFile :: String -> IO FileData
readNamedFile name = do
    contents <- readFile name
    return (FileData name (lines contents))

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
