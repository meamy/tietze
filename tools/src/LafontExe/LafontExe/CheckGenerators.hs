-- | Implementation of check_generators.

module LafontExe.CheckGenerators where

import System.Directory
import Lafont.Generators.Semantics
import Lafont.Parse.GeneratorFile
import LafontExe.Logging.LineBased

-- | Generator logging function intended for use with foldGens. Consumes a generator
-- symbol/semantic pair, together with the result so far (str). Appends the name of the
-- generator, and a textual representation if its semantics (if present), to str. The new
-- string is returned.
logGenerator :: (Show a) => (String, Maybe a) -> String -> String
logGenerator (name, (Just semv)) str = str ++ name ++ " := " ++ (show semv) ++ "\n"
logGenerator (name, Nothing)     str = str ++ name ++ " := (no semantic value)\n"

-- | Consumes a semantic model (sem) and a dictionary of generators (dict). Returns a
-- textual representation of the generators and their semantics.
logGenerators :: (Show a) => SemModel -> GenDict a -> String
logGenerators sem dict = foldGens logGenerator semstr dict
    where semstr = "Semantic Model : " ++ (show sem) ++ "\n"

-- | Consumes the name of a generator file (fnmae) and its contents split at each new
-- line (lines). If the lines parse correctly, then returns a textual representation of
-- the generators and their semantics. Otherwise, the textual representation of a parsing
-- error is returned.
processGeneratorLines :: String -> [String] -> String
processGeneratorLines fname lines =
    case (parseGenFileAsDict lines 0) of
        Left (errLn, err)               -> (logEitherMsg fname errLn err)
        Right (MonoidalGenSummary dict) -> logGenerators MonoidalSem dict 

-- | Consumes the name of a generator file (fname). If the generator file parses
-- correctly, then an internal representation of the generators is printed. Otherwise, a
-- parsing error is printed with file name and line number.
checkGenerators :: String -> IO ()
checkGenerators fname = do
    exists <- doesFileExist fname
    if exists
    then do
        content <- readFile fname
        putStr (processGeneratorLines fname (lines content))
    else putStr ("File does not exist: " ++ fname ++ "\n")
