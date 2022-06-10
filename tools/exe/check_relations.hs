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

-- | Parses all generators and rules, then prints a textual representation of the result.
readFiles :: String -> String -> IO ()
readFiles genFname relFname = do
    genContent <- readFile genFname
    relContent <- readFile relFname
    case (parseGenFileAsAlphabet (lines genContent) 0) of
        Left (errLn, err) -> putStr (logEitherMsg genFname errLn err)
        Right gens        -> putStr (processRelFile relFname gens (lines relContent))

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
