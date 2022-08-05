-- | Functions to log primitive types with a special format.

module LafontExe.Logging.Primitive where

import Lafont.Common
import Lafont.Rewrite.Rules
import Lafont.Rewrite.Lookup

-----------------------------------------------------------------------------------------
-- * Array-Based Logging.

-- | Converts a monoidal word into a dot concatenated string. Symbols are displayed using
-- their display function. Empty words are displayed as ε.
logWord :: MonWord -> String
logWord []          = "ε"
logWord (symb:[])   = (display symb)
logWord (symb:word) = (display symb) ++ "." ++ (logWord word)

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

-----------------------------------------------------------------------------------------
-- * Dictionary-based Logging.

-- | Converts a dictionary of rules into a string. Each line of the string is an ASCII
-- representation of the relation.
logRuleDict :: RuleDict -> String
logRuleDict dict = foldRules (\g str -> (logRule g) ++ "\n" ++ str) "" dict
