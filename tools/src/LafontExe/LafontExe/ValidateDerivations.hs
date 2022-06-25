-- | Implementation of validate_derivation.

module LafontExe.ValidateDerivations where

import System.IO
import Lafont.Common
import Lafont.Rewrite.Lookup
import Lafont.Rewrite.Rules
import Lafont.Rewrite.Simplification
import Lafont.Rewrite.Summary
import Lafont.Parse.GeneratorFile
import Lafont.Parse.RelationFile
import Lafont.Parse.DerivationFile
import LafontExe.IO.Files
import LafontExe.Logging.LineBased
import LafontExe.Logging.Primitive

-----------------------------------------------------------------------------------------
-- * Helpers.

-- | Helper types to simplify code.
type NamedPreDerivation = (String, PreDerivation)
type NamedDerivation = (String, Derivation)
type ListParseRV a = Either (String, Int, DFPError) [a]

-- | Consumes the name of a derivation file (fname), the word obtained from a derivation
-- (act), and the expected word from the end of the file (exp). Returns a string
-- describing the error.
describeIncorrectResult :: String -> MonWord -> MonWord -> String
describeIncorrectResult fname exp act = fstLine ++ sndLine
    where expStr = logWord exp
          actStr = logWord act
          fstLine = "Failed to validate " ++ fname ++ ".\n"
          sndLine = "Expected " ++ expStr ++ " but produced " ++ actStr ++ ".\n"

-- | Consumes the name of a derivation file (fname), the word obtain when a rewrite rule
-- failed to apply (act), and the step number associated with this rewrite (step).
-- Returns a string describing the error.
describeIncorrectStep :: String -> MonWord -> Int -> String
describeIncorrectStep fname act step = fstLine ++ sndLine
    where actStr = logWord act
          stepStr = (show step)
          fstLine = "Failed to validate " ++ fname ++ ".\n"
          sndLine = "Obtained " ++ actStr ++ " at step " ++ stepStr ++ ".\n"

-----------------------------------------------------------------------------------------
-- * Logic.

-- | Consumes a list of pairs, where each tuple contains the name of a derivation file
-- and the Derivation it describes. If a derivation is invalid, then a summary of the
-- failure is printed. Otherwise, a success message is printed.
verifyDerivations :: [NamedDerivation] -> String
verifyDerivations []                            = "Success.\n"
verifyDerivations ((fname, derivation):derivations) = do
    if (success res)
    then if ((output res) == (final sum))
         then verifyDerivations derivations
         else describeIncorrectResult fname (final sum) (output res)
    else describeIncorrectStep fname (output res) (step res)
    where sum = (summary derivation)
          res = simplify (initial sum) (rewrites derivation)

-- | Consumes a list of derivation files and a list of known generators (gens). If all
-- derivations preparse correctly, then returns a list of pairs, where each pair contains
-- the name of a file and the PreDerivation it describes. Otherwise, a parsing error is
-- returned.
readDerivationFiles :: [String] -> [String] -> IO (ListParseRV NamedPreDerivation)
readDerivationFiles []             _    = return (Right [])
readDerivationFiles (fname:fnames) gens = do
    content <- readFile fname
    case (preparseDerivationFile gens (lines content) 0) of
        Left (errLn, err) -> return (Left (fname, errLn, err))
        Right pre         -> do
            ioRes <- readDerivationFiles fnames gens
            case ioRes of
                Left err  -> return (Left err)
                Right res -> return (Right ((fname, pre):res))

-- Consumes a dictionary of rewrite rules (rules) and a list of pairs, where each pair
-- contains the name of a file and the PreDerivation data it describes. If each piece of
-- PreDerivation data is either unnamed or has a unique name, then a new dictionary is
-- returned as obtained by adding each derived rule to rules. Otherwise, the file name of
-- the first derivation with a duplication relation name is returned.
addDerivedRules :: RuleDict -> [NamedPreDerivation] -> Either String RuleDict
addDerivedRules rules []                  = Right rules
addDerivedRules rules ((fname, pre):rest) =
    case (addDerivedRules rules rest) of
        Left fname     -> Left fname
        Right recRules -> case (summary `addSummaryToRules` recRules) of
            Nothing           -> Left fname
            Just updatedRules -> Right updatedRules
    where summary = parsed pre

-- | Consumes a dictionary of rewrite rules (rules) and a list of pairs, where each pair
-- contains the name of a file and the PreDerivation data it describes. If all files
-- parse correctly, then returns a list of pairs, where each pair contains the name of a
-- pair and the Derivation it describes. Otherwise, a parsing error is returned. Requires
-- that all derived rules have already been recorded in rules
parseRewriteSections :: RuleDict -> [NamedPreDerivation] -> ListParseRV NamedDerivation
parseRewriteSections _     []                  = Right []
parseRewriteSections rules ((fname, pre):rest) =
    case (parseDerivationFile rules pre) of
        Left (errLn, err) -> Left (fname, errLn, err)
        Right deriv       -> case (parseRewriteSections rules rest) of
            Left err  -> Left err
            Right res -> Right ((fname, deriv):res)

-- | Consumes a handle, a list of derivation files (DerivFnames), a dictionary of rewrite
-- rules (rules), and a list of generators (gens). If all derivations parse correctly,
-- then the derivations are validated and any invalid derivations are printed to the
-- handle. Otherwise, a parsing error is printed to the handle with file name and line
-- number.
processDerivationFiles :: Handle -> [String] -> RuleDict -> [String] -> IO ()
processDerivationFiles hdl fnames rules gens = do
    readResult <- readDerivationFiles fnames gens
    case readResult of
        Left (fname, errLn, err) -> hPutStr hdl (logEitherMsg fname errLn err)
        Right preDerivations     -> case (addDerivedRules rules preDerivations) of
            Left fname     -> hPutStr hdl (logFromFile fname 0 "Duplicated rule name.")
            Right sumRules -> case (parseRewriteSections sumRules preDerivations) of
                Left (fname, errLn, err) -> hPutStr hdl (logEitherMsg fname errLn err)
                Right derivations        -> hPutStr hdl (verifyDerivations derivations)

-- | Consumes a handle, the name of a relation file (relname), a list of derivation files
-- (derivFnames), and a list of known generators (gens). If all files parse correctly,
-- then the derivations are validated and any invalid derivations are printed to the
-- handle. Otherwise, a parsing error is printed to the handle with file name and line
-- number.
validateDerivationsGivenGens :: Handle -> String -> [String] -> [String] -> IO ()
validateDerivationsGivenGens hdl relFname derivFnames gens = do
    content <- readFile relFname
    case (parseRelFile gens (lines content) 0) of
        (Left (errLn, err)) -> hPutStr hdl (logEitherMsg relFname errLn err)
        (Right dict)        -> processDerivationFiles hdl derivFnames dict gens

-- | See validateDerivations. Requires that both files exist, whereas validateDerivations
-- does not imporse this assumption
validateDerivationsImpl :: Handle -> String -> String  -> [String] -> IO ()
validateDerivationsImpl hdl genFname relFname derivFnames = do
    content <- readFile genFname
    case (parseGenFileAsAlphabet (lines content) 0) of
        Left (errLn, err) -> hPutStr hdl (logEitherMsg genFname errLn err)
        Right gens        -> validateDerivationsGivenGens hdl relFname derivFnames gens

-- | Consumes a handle, the name of a generator file (genFname), the name of a relation
-- file (relFname), and list of derivation file names (derivFnames). If all files parse
-- correctly, then the derivations are validated and any invalid derivations are printed
-- to the handle. Otherwise, a parsing error is printed to the handle with file name and
-- line number.
validateDerivations :: Handle -> String -> String -> [String] -> IO ()
validateDerivations hdl genFname relFname derivFnames = do
    res <- doFilesExist (genFname:(relFname:derivFnames))
    case res of
        Just name -> hPutStr hdl ("File does not exist: " ++ name ++ "\n")
        Nothing   -> validateDerivationsImpl hdl genFname relFname derivFnames
