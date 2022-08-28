-- | Implementation of validate_derivation.

module LafontExe.ValidateDerivations where

import           Data.Maybe
import           Lafont.Either
import           Lafont.Graph
import           Lafont.Parse.DerivationFile
import           Lafont.Rewrite.Abstraction
import           Lafont.Rewrite.Derivations
import           Lafont.Rewrite.Lookup
import           Lafont.Rewrite.Rules
import           Lafont.Rewrite.Simplification
import           Lafont.Rewrite.Summary
import           LafontExe.IO.Files
import           LafontExe.Logging.ErrorFormat
import           LafontExe.Logging.LineBased
import           LafontExe.Logic.Relations
import           System.IO

-----------------------------------------------------------------------------------------
-- * Helpers.

-- | Helper types to simplify code.
type NamedData a = (String, Int, a)
type NamedPreDerivation = NamedData PreDerivation
type NamedAbsDerivation = NamedData AbsDerivation
type NamedDerivation = NamedData Derivation
type ListParseRV a = Either (String, Int, DFPError) [a]

-- | Displays an unmet dependency as a human-readable string.
printUnmetDep :: UnmetDep -> String
printUnmetDep (UnmetDep ""  dst) = dst
printUnmetDep (UnmetDep src dst) = src ++ " -> " ++ dst

-- | Displays a cycle as a humnan-readable list. The list starts on a new line.
printCycle :: DepCycle -> String
printCycle = foldPath f ""
    where f n v str = "\n" ++ show (n + 1) ++ ". " ++ v ++ str

-- | Appends an unnamed list to a named list.
addToNamedList :: String -> [NamedData a] -> [a] -> Int -> [NamedData a]
addToNamedList _    named []          _   = named
addToNamedList name named (a:unnamed) num = (name, num, a) : rest
    where rest = addToNamedList name named unnamed (num + 1)

-- | Extracts the third element from a triple.
third :: (a, b, c) -> c
third (_, _, x) = x

-----------------------------------------------------------------------------------------
-- * Logic.

-- | Folds concretization across all named derivations.
concretize :: DerivationMetadata -> [NamedAbsDerivation] -> Either (String, Int, Int) [NamedDerivation]
concretize _    []                            = Right []
concretize meta ((fname, num, x):derivations) =
    case concretizeDerivation meta x of
        Left pos         -> Left (fname, num, pos)
        Right derivation -> updateRight (concretize meta derivations)
                                        (\rest -> (fname, num, derivation) : rest)

-- | Consumes a list of pairs, where each tuple contains the name of a derivation file
-- and the Derivation it describes. If the dependency graph induced by the list of
-- derivations is invalid, then the error is printed. Otherwise, if a step in a
-- derivation is invalid, then a summary of the failure is printed. Otherwise, a success
-- message is printed.
verifyDerivations :: [NamedAbsDerivation] -> String
verifyDerivations named =
    case detectDerivationError absDerivations of
        Just (Left unmet)  -> "Unmet dependency: " ++ printUnmetDep unmet ++ "\n"
        Just (Right cycle) -> "Dependency cycle detected: " ++ printCycle cycle ++ "\n"
        Nothing            -> case concretize (dmap, emap) named of
            Left (fname, num, pos) -> describeFailedApply fname num pos
            Right derivations      -> verifyDerivationSteps derivations
    where absDerivations = map third named
          dmap = makeDerivationMap absDerivations
          emap = identifyEquationalRules dmap

-- | Consumes a list of pairs, where each tuple contains the name of a derivation file
-- and the Derivation it describes. If a derivation is invalid, then a summary of the
-- failure is printed. Otherwise, a success message is printed.
verifyDerivationSteps :: [NamedDerivation] -> String
verifyDerivationSteps []                                     = "Success.\n"
verifyDerivationSteps ((fname, num, derivation):derivations) =
    if success res
    then if output res == final summary
         then verifyDerivationSteps derivations
         else describeIncorrectResult fname num (final summary) (output res)
    else describeIncorrectStep fname num (output res) (step res)
    where Derivation summary rewrites = derivation
          res = simplify (initial summary) rewrites

-- | Consumes a list of derivation files and a list of known generators (gens). If all
-- derivations preparse correctly, then returns a list of pairs, where each pair contains
-- the name of a file and the PreDerivation it describes. Otherwise, a parsing error is
-- returned.
readDerivationFiles :: [String] -> [String] -> IO (ListParseRV NamedPreDerivation)
readDerivationFiles []             _    = return (Right [])
readDerivationFiles (fname:fnames) gens = do
    content <- readFile fname
    case preparseDerivationFile gens (lines content) 0 of
        Left (errLn, err) -> return (Left (fname, errLn, err))
        Right preList     -> do
            ioRes <- readDerivationFiles fnames gens
            return (updateRight ioRes (\res -> addToNamedList fname res preList 0))

-- Consumes a dictionary of rewrite rules (rules) and a list of named PreDerivations. If
-- each PreDerivation summary is either unnamed or has a unqiue name (with respect to the
-- relations and other PreDerivations), then a set of PreDerivation summary names is
-- returned. Otherwise, the file name of the first PreDerivation with a duplicate summary
-- name is returned.
listDerivedRules :: RuleDict -> [NamedPreDerivation] -> Either (String, Int) DRuleSet
listDerivedRules _     []                       = Right nullRuleSet
listDerivedRules rules ((fname, num, pre):rest) =
    case listDerivedRules rules rest of
        Left  err -> Left err
        Right set -> case addSummaryToSymbols rules set summary of
            Nothing   -> Left (fname, num)
            Just set' -> Right set'
    where summary = parsed pre

-- | Consumes a dictionary of rewrite rules (rules) and a list of pairs, where each pair
-- contains the name of a file and the PreDerivation data it describes. If all files
-- parse correctly, then returns a list of pairs, where each pair contains the name of a
-- pair and the Derivation it describes. Otherwise, a parsing error is returned. Requires
-- that all derived rules have already been recorded in rules
parseRewriteSections :: RuleDict -> DRuleSet -> [NamedPreDerivation] -> ListParseRV NamedAbsDerivation
parseRewriteSections _     _       []                       = Right []
parseRewriteSections rules derived ((fname, num, pre):rest) =
    case parseDerivationFile rules derived pre of
        Left (errLn, err) -> Left (fname, errLn, err)
        Right derivation  -> updateRight (parseRewriteSections rules derived rest)
                                         (\res -> (fname, num, derivation) : res)

-- |
processPreDerivations :: Handle -> [NamedPreDerivation] -> RuleDict -> [String] -> IO ()
processPreDerivations hdl pres rules gens =
    case listDerivedRules rules pres of
        Left (fname, num) -> hPutStr hdl $ logFromFile fname 0 $ reportDupRule num
        Right derived     -> case parseRewriteSections rules derived pres of
            Left (fname, errLn, err) -> hPutStr hdl $ logEitherMsg fname errLn err
            Right derivations        -> hPutStr hdl $ verifyDerivations derivations

-- | Consumes a handle, a list of derivation files (DerivFnames), a dictionary of rewrite
-- rules (rules), and a list of generators (gens). If all derivations parse correctly,
-- then the derivations are validated and any invalid derivations are printed to the
-- handle. Otherwise, a parsing error is printed to the handle with file name and line
-- number.
processDerivationFiles :: Handle -> [String] -> RuleDict -> [String] -> IO ()
processDerivationFiles hdl fnames rules gens = do
    readResult <- readDerivationFiles fnames gens
    case readResult of
        Left (fname, errLn, err) -> hPutStr hdl $ logEitherMsg fname errLn err
        Right pres               -> processPreDerivations hdl pres rules gens

-- | See validateDerivations. Requires that both files exist, whereas validateDerivations
-- does not imporse this assumption
validateDerivationsImpl :: Handle -> String -> String  -> [String] -> IO ()
validateDerivationsImpl hdl genFname relFname derivFnames = do
    genFile <- readNamedFile genFname
    relFile <- readNamedFile relFname
    case readGeneratorsAndRules genFile relFile of
        UnknownSem             -> hPutStr hdl "Impl Error: Unknown semantic model."
        BadGenFile fn ln err   -> hPutStr hdl $ logEitherMsg fn ln err
        BadRelFile fn ln err   -> hPutStr hdl $ logEitherMsg fn ln err
        InvalidRel rname       -> hPutStr hdl $ reportInvalidRule rname
        MissingGen rname       -> hPutStr hdl $ reportUnknownGen rname
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
