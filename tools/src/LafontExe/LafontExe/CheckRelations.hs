-- | Implementation of check_relations.

module LafontExe.CheckRelations where

import           LafontExe.IO.Files
import           LafontExe.Logging.ErrorFormat
import           LafontExe.Logging.LineBased
import           LafontExe.Logging.Primitive
import           LafontExe.Logic.Relations
import           System.IO

-----------------------------------------------------------------------------------------
-- * Logic.

-- | See checkRelations. Requires that both files exist, whereas checkRelations does not
-- imporse this assumption
checkRelationsImpl :: Handle -> String -> String -> IO ()
checkRelationsImpl hdl genFname relFname = do
    genFile <- readNamedFile genFname
    relFile <- readNamedFile relFname
    case readGeneratorsAndRules genFile relFile of
        UnknownSem           -> hPutStr hdl "Impl Error: Unknown semantic model."
        BadGenFile fn ln err -> hPutStr hdl $ logEitherMsg fn ln err
        BadRelFile fn ln err -> hPutStr hdl $ logEitherMsg fn ln err
        InvalidRel rname     -> hPutStr hdl $ reportInvalidRule rname
        MissingGen rname     -> hPutStr hdl $ reportUnknownGen rname
        GenRulePair _ rules  -> hPutStr hdl $ logRuleDict rules

-- | Consumes a handle, the name of a generator file (genFname) and the name of a
-- relation file (relFname). If the generator and relation files parse correctly, then an
-- internal representation of the generators is printed to handle. Otherwise, a parsing
-- error is printed to handle with the file name and line number.
checkRelations :: Handle -> String -> String -> IO ()
checkRelations hdl genFname relFname = do
    res <- doFilesExist [genFname, relFname]
    case res of
        Just name -> putStrLn ("File does not exist: " ++ name)
        Nothing   -> checkRelationsImpl hdl genFname relFname
