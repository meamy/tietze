-- | This module provides types and functions for maintain collections of rewrite rules.

module Lafont.Rewrite.Lookup (
    RuleDict,
    empty,
    hasRule,
    addRule,
    foldRules,
    interpretRule
) where

import qualified Data.Map             as Map
import           Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- * Rule Dictionary.

-- | A mapping from rule names (strings) to the rewrite rules they represent.
newtype RuleDict = RuleDict (Map.Map String RewriteRule) deriving (Eq,Show)

-- | Creates an empty RuleDict.
empty :: RuleDict
empty = RuleDict Map.empty

-- | Returns true if a rule is already recorded.
hasRule :: RuleDict -> String -> Bool
hasRule (RuleDict dict) id = Map.member id dict

-- | Records a identifier/rule pair inside a rewrite rule dictionary.
addRule :: RuleDict -> (String, RewriteRule) -> RuleDict
addRule (RuleDict dict) (id, rule) = RuleDict (Map.insert id rule dict)

-- | Folds f over the (name, rule) entries of dict, and returns the accumulated value.
foldRules :: ((String, RewriteRule) -> b -> b) -> b -> RuleDict -> b
foldRules f v (RuleDict dict) = Map.foldrWithKey adjust v dict
    where adjust key semv acc = f (key, semv) acc

-- | Returns a rule by its name. If the rule does not exist, then nothing is returned. To
-- check if a generator is recorded, then use (hasRule dict id).
interpretRule :: RuleDict -> String -> Maybe RewriteRule
interpretRule (RuleDict dict) id = Map.lookup id dict
