-- | Implementation of check_relations.

module LafontExe.CheckRelations where

import Lafont.Rewrite.Lookup
import Lafont.Parse.GeneratorFile
import Lafont.Parse.RelationFile
import LafontExe.IO.Files
import LafontExe.Logging.LineBased
import LafontExe.Logging.Primitive

-- | Consumes the name of a relation file (fname), a list of known generators (gens), and
-- the lines of the relation file (lines). If the lines parse correctly with respect to
-- gens, then returns a textual representation of the relations. Otherwise, the textual
-- representation of a parsing error is returned.
processRelationFile :: String -> [String] -> [String] -> String
processRelationFile fname gens lines =
    case (parseRelFile gens lines 0) of
        (Left (errLn, err)) -> logEitherMsg fname errLn err
        (Right dict)        -> foldRules (\g str -> (logRule g) ++ "\n" ++ str) "" dict

-- | See checkRelations. Requires that both files exist, whereas checkRelations does not
-- imporse this assumption
checkRelationsImpl :: String -> String -> IO ()
checkRelationsImpl genFname relFname = do
    gContent <- readFile genFname
    rContent <- readFile relFname
    case (parseGenFileAsAlphabet (lines gContent) 0) of
        Left (errLn, err) -> putStr (logEitherMsg genFname errLn err)
        Right gens        -> putStr (processRelationFile relFname gens (lines rContent))

-- | Consumes the name of a generator file (genFname) and the name of a relation file
-- (relFname). If the generator and relation files parse correctly, then an internal
-- representation of the generators is printed. Otherwise, a parsing error is printed
-- with the file name and line number.
checkRelations :: String -> String -> IO ()
checkRelations genFname relFname = do
    res <- doFilesExist [genFname, relFname]
    case res of
        Just name -> putStr ("File does not exist: " ++ name ++ "\n")
        Nothing   -> checkRelationsImpl genFname relFname
