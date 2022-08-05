-- | Implementation of validate_derivation.

module LafontExe.ValidateDerivations where

import System.IO
import Lafont.Graph
import Lafont.Rewrite.Derivations
import Lafont.Rewrite.Lookup
import Lafont.Rewrite.Rules
import Lafont.Rewrite.Simplification
import Lafont.Rewrite.Summary
import Lafont.Parse.DerivationFile
import LafontExe.IO.Files
import LafontExe.Logic.Relations
import LafontExe.Logging.ErrorFormat
import LafontExe.Logging.LineBased

-----------------------------------------------------------------------------------------
-- * Helpers.

-- | Helper types to simplify code.
type NamedPreDerivation = (String, PreDerivation)
type NamedDerivation = (String, Derivation)
type ListParseRV a = Either (String, Int, DFPError) [a]

-- | Displays an unmet dependency as a human-readable string.
printUnmetDep :: UnmetDep -> String
printUnmetDep (UnmetDep ""  dst) = dst
printUnmetDep (UnmetDep src dst) = src ++ " -> " ++ dst

-- | Displays a cycle as a humnan-readable list. The list starts on a new line.
printCycle :: DepCycle -> String
printCycle walk = foldPath f "" walk
    where f n v str = "\n" ++ (show (n + 1)) ++ ". " ++ v ++ str

-----------------------------------------------------------------------------------------
-- * Logic.

-- | Consumes a list of pairs, where each tuple contains the name of a derivation file
-- and the Derivation it describes. If the dependency graph induced by the list of
-- derivations is invalid, then the error is printed. Otherwise, if a step in a
-- derivation is invalid, then a summary of the failure is printed. Otherwise, a success
-- message is printed.
verifyDerivations :: [NamedDerivation] -> String
verifyDerivations derivations =
    case (detectDerivationError (map snd derivations)) of
        Just (Left unmet)  -> "Unmet dependency: " ++ (printUnmetDep unmet) ++ "\n"
        Just (Right cycle) -> "Dependency cycle detected: " ++ (printCycle cycle) ++ "\n"
        Nothing            -> verifyDerivationSteps derivations

-- | Consumes a list of pairs, where each tuple contains the name of a derivation file
-- and the Derivation it describes. If a derivation is invalid, then a summary of the
-- failure is printed. Otherwise, a success message is printed.
verifyDerivationSteps :: [NamedDerivation] -> String
verifyDerivationSteps []                                = "Success.\n"
verifyDerivationSteps ((fname, derivation):derivations) = do
    if (success res)
    then if ((output res) == (final sum))
         then verifyDerivationSteps derivations
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

-- | See validateDerivations. Requires that both files exist, whereas validateDerivations
-- does not imporse this assumption
validateDerivationsImpl :: Handle -> String -> String  -> [String] -> IO ()
validateDerivationsImpl hdl genFname relFname derivFnames = do
    genFile <- readNamedFile genFname
    relFile <- readNamedFile relFname
    case (readGeneratorsAndRules genFile relFile) of
        UnknownSem             -> hPutStr hdl "Impl Error: Unknown semantic model."
        BadGenFile fn ln err   -> hPutStr hdl (logEitherMsg fn ln err)
        BadRelFile fn ln err   -> hPutStr hdl (logEitherMsg fn ln err)
        InvalidRel rname       -> hPutStr hdl (reportInvalidRule rname)
        MissingGen rname       -> hPutStr hdl (reportUnknownGen rname)
        GenRulePair gens rules -> processDerivationFiles hdl derivFnames rules gens

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
