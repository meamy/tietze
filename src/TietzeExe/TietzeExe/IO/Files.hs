-- | Helper functions to interace with files.

module TietzeExe.IO.Files
  ( FileData (..)
  , ParseFilesRV (..)
  , readNamedFile
  , readNamedFiles
  , readDerivationFiles
  , doFilesExist
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import System.Directory (doesFileExist)
import Tietze.Either (updateRight)
import Tietze.Named
  ( Named (..)
  , addToNamedList
  )
import Tietze.Parse.DerivationFile (preparseDerivationFile)

-----------------------------------------------------------------------------------------
-- * General-Purpose Reading/Writing.

-- | Pairs together the name of a file, and its contents.
data FileData = FileData String [String]

-- | Consumes the name of a file. If the file is readable, then returns its FileData.
readNamedFile :: String -> IO FileData
readNamedFile name = do
    contents <- readFile name
    return $ FileData name $ lines contents

-- | Consumes a list of files. If the files are readable, then returns its FileData.
readNamedFiles :: [String] -> IO [FileData]
readNamedFiles []           = return []
readNamedFiles (name:names) = do
    contents <- readNamedFile name
    rstOfLst <- readNamedFiles names
    return $ contents:rstOfLst

-----------------------------------------------------------------------------------------
-- * Specialized Reading/Writing.

-- | Function to parse a structured file. Takes as input the lines of a file (as a list
-- of strings) and the current line number. Returns either a line number and error, or a
-- list of parsed elements.
type Parser a b = [String] -> Int -> Either (Int, a) [b]

-- | Represents a parsing error (file name, line number, and error type) or a list of
-- parsed elements associated with their source file names.
type ParseFilesRV a b = Either (String, Int, a) [Named b]

-- | Takes as input a structured file parser and a list of paths to structured files.
-- Returns a parsing result, as described by the ParseFileRV type.
readStructuredFiles :: Parser a b -> [String] -> IO (ParseFilesRV a b)
readStructuredFiles _ []             = return $ Right []
readStructuredFiles f (fname:fnames) = do
    content <- readFile fname
    case f (lines content) 1 of
        Left (ln, err) -> return $ Left (fname, ln, err)
        Right pcontent -> do
            parsed <- readStructuredFiles f fnames
            return $ updateRight parsed $ \res -> addToNamedList fname res pcontent 1

-- | Specializations.
readDerivationFiles gens = readStructuredFiles $ preparseDerivationFile gens

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
    else return $ Just filename
