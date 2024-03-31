-- | Data types and functions to facilitate semantic evaluation of rules.

module Tietze.Generators.RuleSem
  ( RuleDictStatus (..)
  , isRuleDictStatusGood
  , checkRuleSem
  ) where
 
-----------------------------------------------------------------------------------------
-- * Import Section.

import Tietze.Generators.Categories (MonoidObj)
import Tietze.Generators.Semantics
  ( GenDict
  , semComp
  )
import Tietze.Rewrite.Lookup
  ( RuleDict
  , foldRules
  )
import Tietze.Rewrite.Rules (RewriteRule (..))

-----------------------------------------------------------------------------------------
-- * Data types and functions to validate rule dictionaries.

-- | A datatype to either indicate that a rule dictionary is semantically valid, or
-- identify the first invalid rule in the dictionary.
data RuleDictStatus = GoodRuleDict
                    | InvalidRuleSem String
                    | IncompleteGenSet String
                    deriving (Show, Eq)

-- | Returns true if a RuleDictStatus is GoodRuleDict.
isRuleDictStatusGood :: RuleDictStatus -> Bool
isRuleDictStatusGood GoodRuleDict = True
isRuleDictStatusGood _            = False

-- | Consumes a mapping of generators to monoid semantic values and a dictionary of
-- rules (dict). If there is an invalid rule in dict, with respect to composition and
-- identity, then InvalidRuleSem is returned with the name of the rule. If there is a
-- missing generator for a rule in dict, then IncompleteGenSet is returned with the name
-- of the rule. Otherwise, GoodRuleDict is returned.
checkRuleSem :: (MonoidObj a) => GenDict a -> RuleDict -> RuleDictStatus
checkRuleSem gens = foldRules fn GoodRuleDict
    where fn (name, rule) res = if isRuleDictStatusGood res
                                then case semComp gens (lhs rule) (rhs rule) of
                                    Nothing    -> IncompleteGenSet name
                                    Just False -> InvalidRuleSem name
                                    Just True  -> GoodRuleDict
                                else res
