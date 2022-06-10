-- | Command-line application to check the syntax of a relation file (with respect to a
-- generator file), and then displays a representation of each rewrite rule.

module Main where

import System.Directory
import System.Environment
import DyadicRewrite.Common
import DyadicRewrite.Rewrite.Rules
import DyadicRewrite.Rewrite.Lookup
import DyadicRewrite.Generators.Semantics
import DyadicRewrite.IO.Files
import DyadicRewrite.IO.LineBasedLogging
import DyadicRewrite.Parse.Semantics
import DyadicRewrite.Parse.GeneratorFile
import DyadicRewrite.Parse.RelationFile

-- | Converts a monoidal word into a dot concatenated string.
logWord :: MonWord -> String
logWord []          = "ε"
logWord (symb:[])   = (show symb)
logWord (symb:word) = (show symb) ++ "." ++ (logWord word)

-- | Converts a rewrite rule into a string.
logRule :: (String, RewriteRule) -> String
logRule (name, rule) = name ++ ": " ++ lstr ++ " " ++ ostr ++ " " ++ rstr ++ "\n"
    where lstr = logWord (lhs rule)
          ostr = if (equational rule) then "=" else "→"
          rstr = logWord (rhs rule)

-- | Processes all lines of a relation file, and returns a textual representation of the
-- relations. If parsing fails, then a formatted error message is returned.
processRelFile :: String -> [String] -> [String] -> String
processRelFile fname gens lines =
    case (parseRelFile gens lines 0) of
        (Left (errLn, err)) -> logEitherMsg fname errLn err
        (Right dict)        -> foldRules (\gen str -> (logRule gen) ++ str) "" dict

-- | Factors out the generator processing code so that it does not depend on the semantic
-- model's value type.
summarizeGens :: String -> SemParser a -> [String] -> Int -> Either String [String]
summarizeGens fname parser gens ln = case (parseGenFile parser gens ln) of
    (Left (errLn, err)) -> Left (logEitherMsg fname errLn err)
    (Right dict)        -> Right (toAlphabet dict)

-- | Processes all lines of a generator file, and returns a list of the generators. If
-- parsing fails, then a formatted error message is returned.
processGenFile :: String -> [String] -> Either String [String]
processGenFile fname lines =
    case (parseSemanticModel lines 0) of
        Left (errLn, err)        -> Left (logEitherMsg fname errLn err)
        Right (sem, semLn, gens) -> let nextLn = semLn + 1 in case sem of
            MonoidalSem    -> summarizeGens fname parseMonoidalSem gens nextLn
            DyadicOneSem   -> Left (logFromFile fname semLn "Dyadic(1) not implemented.")
            DyadicTwoSem   -> Left (logFromFile fname semLn "Dyadic(2) not implemented.")
            DyadicThreeSem -> Left (logFromFile fname semLn "Dyadic(3) not implemented.")

-- | Parses all generators and rules, then prints a textual representation of the result.
readFiles :: String -> String -> IO ()
readFiles genFname relFname = do
    genContent <- readFile genFname
    relContent <- readFile relFname
    case (processGenFile genFname (lines genContent)) of
        Left errMsg -> putStr errMsg
        Right gens  -> putStr (processRelFile relFname gens (lines relContent))

-- | Validates file before calling readFiles.
checkFiles :: String -> String -> IO ()
checkFiles genFname relFname = do
    res <- doFilesExist [genFname, relFname]
    case res of
        Just name -> putStr ("File does not exist: " ++ name ++ "\n")
        Nothing   -> readFiles genFname relFname

-- | Parses and validates arguments before calling checkFiles.
main = do
    pname <- getProgName
    args <- getArgs
    if (not ((length args) == 2))
    then putStr ("usage: " ++ pname ++ " gen_file rel_file\n")
    else checkFiles (args !! 0) (args !! 1)
