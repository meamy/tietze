-- | This module provides types and functions for maintain collections of rewrite rules.

module DyadicRewrite.Rewrite.Lookup where

import qualified Data.Map
import DyadicRewrite.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- * Rule Dictionary.

-- | A mapping from rule names (strings) to the rewrite rules they represent.
type RuleDict = Data.Map.Map String RewriteRule

-- | Creates an empty RuleDict.
empty :: RuleDict
empty = Data.Map.empty

-- | Returns true if a rule is already recorded.
hasRule :: RuleDict -> String -> Bool
hasRule dict id = Data.Map.member id dict

-- | Records a identifier/rule pair inside a rewrite rule dictionary.
addRule :: RuleDict -> (String, RewriteRule) -> RuleDict
addRule dict (id, rule) = Data.Map.insert id rule dict

-- | Returns a rule by its name. If the rule does not exist, then nothing is returned. To
-- check if a generator is recorded, then use (hasRel dict id).
interpretRule :: RuleDict -> String -> Maybe RewriteRule
interpretRule dict id = Data.Map.lookup id dict
