-- | Data types and functions to facilitate semantic evaluation of relations.

module Lafont.Generators.RuleSem where

import Lafont.Generators.Semantics
import Lafont.Rewrite.Lookup
import Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- * Data types and functions to validate rule dictionaries.

-- | 
data RuleDictStatus = GoodRuleDict
                    | InvalidRuleSem String
                    | IncompleteGenSet String
                    deriving (Eq,Show)

-- | Consumes a composition function, an identity element, and a mapping of generators
-- to semantic values. Returns a rule-foldable function that implements checkRuleSem. See
-- checkRuleSem for details.
foldRelSem :: (Eq a) => (a -> a -> a) -> a -> GenDict a -> RuleDictFoldFn RuleDictStatus
foldRelSem compose id gens (name, rule) GoodRuleDict =
    case (semComp compose id gens lword rword) of
        Nothing    -> IncompleteGenSet name
        Just False -> InvalidRuleSem name
        Just True  -> GoodRuleDict
    where lword = lhs rule
          rword = rhs rule
foldRelSem _ _ _ _ res = res

-- | Consumes a composition function, an identity element, a mapping of generators to
-- semantic values, ad a dictionary of rules (dict). If there is an invalid rule in dict,
-- with respect to composition and identity, then InvalidRuleSem is returned with the
-- name of the rule. If there is a missing generator for a rule in dict, then
-- IncompleteGenSet is returned with the name of the rule. Otherwise, GoodRuleDict is
-- returned.
checkRuleSem :: (Eq a) => (a -> a -> a) -> a -> GenDict a -> RuleDict -> RuleDictStatus
checkRuleSem compose id gens dict = foldRules f GoodRuleDict dict
    where f = foldRelSem compose id gens
