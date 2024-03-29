-- | Functions to log primitive types with a special format.

module TietzeExe.Logging.Primitive
  ( logEIRule
  , logEIRuleByQuery
  , logEIView
  , logRule
  , logRuleDict
  , logWord
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.Set as Set
import Tietze.Common
  ( Display (..)
  , MonWord (..)
  , Symbol
  )
import Tietze.Edit.EIRules
  ( EIRule
  , getRelName
  , isDerived
  )
import Tietze.Edit.Invert
  ( EIView
  , findEIRule
  )
import Tietze.Rewrite.Lookup
  ( RuleDict
  , foldRules
  )
import Tietze.Rewrite.Rules (RewriteRule (..))

-----------------------------------------------------------------------------------------
-- * Array-Based Logging.

-- | Converts a monoidal word into a dot concatenated string. Symbols are displayed using
-- their display function. Empty words are displayed as ε.
logWord :: MonWord -> String
logWord []          = "ε"
logWord [symb]      = display symb
logWord (symb:word) = display symb ++ "." ++ logWord word

-----------------------------------------------------------------------------------------
-- * Tuple-Based Logging.

-- | Consumes a tuple (K, V) where V is a rewrite rule and K is its name. Returns a
-- string of the form:
--     <K>: <LHS> <OP> <RHS>
logRule :: (String, RewriteRule) -> String
logRule (name, rule) = name ++ ": " ++ lstr ++ " " ++ ostr ++ " " ++ rstr
    where lstr = logWord (lhs rule)
          ostr = if equational rule then "=" else "→"
          rstr = logWord (rhs rule)

-- | Converts an EIRule to the name of the underlying relation, together with a tag
-- indicating whether or not the relation is derived.
logEIRule :: EIRule -> String
logEIRule rule = getRelName rule ++ dstr
    where dstr = if isDerived rule then " [derived]" else ""

-- | Implementation details for logEIRule. Returns the name of the matching relation,
-- taking into account failed queries.
logEIRuleByQueryImpl :: EIView -> Symbol -> String
logEIRuleByQueryImpl view sym =
    case findEIRule view sym of
        Just rule -> logEIRule rule
        Nothing   -> "(unknown)"

-- | Consumes an EIView and a symbol. Returns a textual representation of the symbol,
-- and the relation EIRule associated with the symbol. In particular, if the rule cannot
-- be found, then "(unknown)" is used as a relation name. In either case, the string is
-- of the form:
--     <SYMBOL_NAME>: <RULE_NAME>
logEIRuleByQuery :: EIView -> Symbol -> String
logEIRuleByQuery view sym = display sym ++ ": " ++ logEIRuleByQueryImpl view sym

-----------------------------------------------------------------------------------------
-- * Dictionary-based Logging.

-- | Converts a dictionary of rules into a string. Each line of the string is an ASCII
-- representation of the rule.
logRuleDict :: RuleDict -> String
logRuleDict = foldRules (\g str -> logRule g ++ "\n" ++ str) ""

-- | Converts an EIView and a set of symbols into a string. Each line of the string is an
-- ASCII representation of the EIRule corresponding to a symbol in the set (according to
-- logEIRuleByQuery).
logEIView :: Set.Set Symbol -> EIView -> String
logEIView symset view = foldl f "" symset
    where f str sym = str ++ logEIRuleByQuery view sym ++ "\n"
