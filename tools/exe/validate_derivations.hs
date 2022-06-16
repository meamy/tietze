-- | Command-line application to verify a list of derivation files.

module Main where

import System.Directory
import System.Environment
import Lafont.Rewrite.Lookup
import Lafont.Rewrite.Simplification
import Lafont.IO.Files
import Lafont.IO.LineBasedLogging
import Lafont.IO.PrimitiveLogging
import Lafont.Parse.GeneratorFile
import Lafont.Parse.RelationFile
import Lafont.Parse.DerivationFile

-- | Helper types to simplify code.
type NamedDerivationData = (String, RewritePreamble, Derivation)
type DerivationFileSummary = Either (String, Int, DFPError) [NamedDerivationData]

-- | Consumes a list of generators and a rule dictionary. Uses these 
readDerivations :: [String] -> RuleDict -> [String] -> IO DerivationFileSummary
readDerivations []             _     _    = return (Right [])
readDerivations (fname:fnames) rules gens = do
    content <- readFile fname
    case (parseDerivationFile rules gens (lines content) 0) of
        Left (errLn, err) -> return (Left (fname, errLn, err))
        Right (pre, sum)  -> do
            ioRes <- readDerivations fnames rules gens
            case ioRes of
                Left err  -> return (Left err)
                Right res -> return (Right ((fname, pre, sum):res))

-- | Consumes a list of named derivations. Attempts to validate all derivations. If a
-- derivation fails to be validated, then the failure is logged. Otherwise, success is
-- logged to the console
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

-- | Consumes a list of generators and a dictionary of rules. Parses all derivations. If
-- the derivations are valid, then success is printed to the console. Otherwise, the
-- first failure is logged.
processDerivationFiles :: [String] -> RuleDict -> [String] -> IO ()
processDerivationFiles fnames rules gens = do
    readResult <- readDerivations fnames rules gens
    case readResult of
        Left (fname, errLn, err) -> putStr (logEitherMsg fname errLn err)
        Right derivations        -> verifyDerivations derivations

-- | Consumes a list of generators. Parses all rules and derivations. If the derivations
-- are valid, then success is printed to the console. Otherwise, the first failure is
-- logged.
processRelAndDerivationFiles :: String -> [String] -> [String] -> IO ()
processRelAndDerivationFiles relFname derivFnames gens = do
    content <- readFile relFname
    case (parseRelFile gens (lines content) 0) of
        (Left (errLn, err)) -> putStr (logEitherMsg relFname errLn err)
        (Right dict)        -> processDerivationFiles derivFnames dict gens

-- | Parses all generators, rules, and derivation. If the derivations are valid, then
-- success is printed to the console. Otherwise, the first failure is logged.
readFiles :: String -> String  -> [String] -> IO ()
readFiles genFname relFname derivFnames = do
    content <- readFile genFname
    case (parseGenFileAsAlphabet (lines content) 0) of
        Left (errLn, err) -> putStr (logEitherMsg genFname errLn err)
        Right gens        -> processRelAndDerivationFiles relFname derivFnames gens

-- | Validates file before calling readFiles.
checkFiles :: String -> String -> [String] -> IO ()
checkFiles genFname relFname derivFnames = do
    res <- doFilesExist (genFname:(relFname:derivFnames))
    case res of
        Just name -> putStr ("File does not exist: " ++ name ++ "\n")
        Nothing   -> readFiles genFname relFname derivFnames

-- | Parses and validates arguments before calling checkFiles.
main = do
    pname <- getProgName
    args <- getArgs
    if (not ((length args) >= 3))
    then putStr ("usage: gen_file rel_file deriv_file ... deriv_file\n")
    else checkFiles (args !! 0) (args !! 1) (tail (tail args))
