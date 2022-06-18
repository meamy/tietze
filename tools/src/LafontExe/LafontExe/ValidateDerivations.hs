-- | Implementation of validate_derivation.

module LafontExe.ValidateDerivations where

import Lafont.Rewrite.Lookup
import Lafont.Rewrite.Simplification
import Lafont.Parse.GeneratorFile
import Lafont.Parse.RelationFile
import Lafont.Parse.DerivationFile
import LafontExe.IO.Files
import LafontExe.Logging.LineBased
import LafontExe.Logging.Primitive

-- | Helper types to simplify code.
type NamedDerivationData = (String, RewritePreamble, Derivation)
type DerivationFileSummary = Either (String, Int, DFPError) [NamedDerivationData]

-- | Consumes a list 3-tuples, where each tuple contains the name of a derivation file,
-- its preamble, and the derivation it describes. If a derivation is invalid, then a
-- summary of the failure is printed. Otherwise, a success message is printed.
verifyDerivations :: [NamedDerivationData] -> IO ()
verifyDerivations []                            = putStr ("Success.\n")
verifyDerivations ((fname, _, sum):derivations) = do
    if (success result)
    then if ((output result) == (final sum))
         then verifyDerivations derivations
         else let exp = logWord (final sum)
                  actual = logWord (output result)
              in do putStr ("Failed to validate " ++ fname ++ ".\n")
                    putStr ("Expected " ++ exp ++ " but obtained " ++ actual ++ ".\n")
    else let actual = logWord (output result)
             stepStr = show (step result)
         in do putStr ("Failed to validate " ++ fname ++ ".\n")
               putStr ("Obtained " ++ actual ++ " at step " ++ stepStr ++ ".\n")
    where result = simplify (initial sum) (rewrites sum)

-- | Consumes a list of derivation files (DerivFnames), a dictionary of rewrite rules
-- (rules), and a list of generators (gens). If all derivations parse correctly, then
-- returns a list of 3-tuples, where each tuple contains the name of a derivation file,
-- its preamble, and the derivation it describes. Otherwise, a parsing error is returned.
readDerivationFiles :: [String] -> RuleDict -> [String] -> IO DerivationFileSummary
readDerivationFiles []             _     _    = return (Right [])
readDerivationFiles (fname:fnames) rules gens = do
    content <- readFile fname
    case (parseDerivationFile rules gens (lines content) 0) of
        Left (errLn, err) -> return (Left (fname, errLn, err))
        Right (pre, sum)  -> do
            ioRes <- readDerivationFiles fnames rules gens
            case ioRes of
                Left err  -> return (Left err)
                Right res -> return (Right ((fname, pre, sum):res))

-- | Consumes a list of derivation files (DerivFnames), a dictionary of rewrite rules
-- (rules), and a list of generators (gens). If all derivations parse correctly, then the
-- derivations are validated and any invalid derivations are printed. Otherwise, a
-- parsing error is printed with file name and line number.
processDerivationFiles :: [String] -> RuleDict -> [String] -> IO ()
processDerivationFiles fnames rules gens = do
    readResult <- readDerivationFiles fnames rules gens
    case readResult of
        Left (fname, errLn, err) -> putStr (logEitherMsg fname errLn err)
        Right derivations        -> verifyDerivations derivations

-- | Consumes the name of a relation file (relname), a list of derivation files
-- (derivFnames), and a list of known generators (gens). If all files parse correctly,
-- then the derivations are validated and any invalid derivations are printed. Otherwise,
-- a parsing error is printed with file name and line number.
validateDerivationsGivenGens :: String -> [String] -> [String] -> IO ()
validateDerivationsGivenGens relFname derivFnames gens = do
    content <- readFile relFname
    case (parseRelFile gens (lines content) 0) of
        (Left (errLn, err)) -> putStr (logEitherMsg relFname errLn err)
        (Right dict)        -> processDerivationFiles derivFnames dict gens

-- | See validateDerivations. Requires that both files exist, whereas validateDerivations
-- does not imporse this assumption
validateDerivationsImpl :: String -> String  -> [String] -> IO ()
validateDerivationsImpl genFname relFname derivFnames = do
    content <- readFile genFname
    case (parseGenFileAsAlphabet (lines content) 0) of
        Left (errLn, err) -> putStr (logEitherMsg genFname errLn err)
        Right gens        -> validateDerivationsGivenGens relFname derivFnames gens

-- | Consumes the name of a generator file (genFname), the name of a relation file
-- (relFname), and list of derivation file names (derivFnames). If all files parse
-- correctly, then the derivations are validated and any invalid derivations are printed.
-- Otherwise, a parsing error is printed with file name and line number.
validateDerivations :: String -> String -> [String] -> IO ()
validateDerivations genFname relFname derivFnames = do
    res <- doFilesExist (genFname:(relFname:derivFnames))
    case res of
        Just name -> putStr ("File does not exist: " ++ name ++ "\n")
        Nothing   -> validateDerivationsImpl genFname relFname derivFnames
