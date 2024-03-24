-- | Implementation of check_generators.

module TietzeExe.CheckGenerators where

import           Lafont.Common
import           Lafont.Generators.Display
import           Lafont.Generators.Semantics
import           Lafont.Parse.GeneratorFile
import           TietzeExe.IO.Files
import           TietzeExe.Logging.LineBased
import           System.Directory
import           System.IO

-----------------------------------------------------------------------------------------
-- * Helpers.

-- | Generator logging function intended for use with foldGens. Consumes a generator
-- symbol/semantic pair, together with the result so far (str). Appends the name of the
-- generator, and a textual representation if its semantics (if present), to str. The new
-- string is returned.
logGenerator :: (Display a) => (String, Maybe a) -> String -> String
logGenerator (name, Just semv) str = str ++ name ++ " := " ++ display semv ++ "\n"
logGenerator (name, Nothing)   str = str ++ name ++ " := (no semantic value)\n"

-- | Consumes a semantic model (sem) and a dictionary of generators (dict). Returns a
-- textual representation of the generators and their semantics.
logGenerators :: (Display a) => SemModel -> GenDict a -> String
logGenerators sem = foldGens logGenerator semstr
    where semstr = "Semantic Model : " ++ display sem ++ "\n"

-----------------------------------------------------------------------------------------
-- * Logic.

-- | Consumes the name of a generator file (fnmae) and its contents split at each new
-- line (lines). If the lines parse correctly, then returns a textual representation of
-- the generators and their semantics. Otherwise, the textual representation of a parsing
-- error is returned.
processGeneratorLines :: FileData -> String
processGeneratorLines (FileData fname lines) =
    case parseGenFileAsDict lines 1 of
        Left (errLn, err)                     -> logEitherMsg fname errLn err
        Right (MonoidGenSummary dict)         -> logGenerators MonoidSem dict
        Right (DyadicTwoSummary dict)         -> logGenerators DyadicTwoSem dict
        Right (DyadicThreeSummary dict)       -> logGenerators DyadicThreeSem dict
        Right (ModMultProductSummary dict ps) -> logGenerators (MultModPSem ps) dict
        Right (ModAddProductSummary dict ps)  -> logGenerators (AddModPSem ps) dict
        _                                     -> "Semantic model not supported.\n"

-- | Consumes a handle, the name of a generator file (fname). If the generator file
-- parses correctly, then an internal representation of the generators is printed to the
-- handle. Otherwise, a parsing error is printed to the handle with file name and line
-- number.
checkGenerators :: Handle -> String -> IO ()
checkGenerators hdl fname = do
    exists <- doesFileExist fname
    if exists
    then do
        content <- readNamedFile fname
        hPutStr hdl $ processGeneratorLines content
    else putStrLn ("File does not exist: " ++ fname)
