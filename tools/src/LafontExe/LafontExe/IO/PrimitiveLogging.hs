-- | Functions to log primitive types with a special format.

module LafontExe.IO.PrimitiveLogging where

import Lafont.Common
import Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- * Array-Based Logging.

-- | Converts a monoidal word into a dot concatenated string. Symbols are displayed using
-- their show function. Empty words are displayed as ε.
logWord :: MonWord -> String
logWord []          = "ε"
logWord (symb:[])   = (show symb)
logWord (symb:word) = (show symb) ++ "." ++ (logWord word)

-----------------------------------------------------------------------------------------
-- * Tuple-Based Logging.

-- | Consumes a tuple (K, V) where V is a rewrite rule and L is its name. Returns a
-- string of the form:
--     <K>: <LHS> <OP> <RHS>
logRule :: (String, RewriteRule) -> String
logRule (name, rule) = name ++ ": " ++ lstr ++ " " ++ ostr ++ " " ++ rstr
    where lstr = logWord (lhs rule)
          ostr = if (equational rule) then "=" else "→"
          rstr = logWord (rhs rule)
