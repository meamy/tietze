-- | This module provides types and functions for maintain collections of rewrite rules.

module DyadicRewrite.Rewrite.Lookup where

import qualified Data.Map
import DyadicRewrite.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- * Relation Dictionary.

-- | A mapping from relation names (strings) to the relations they represent.
type RelDict = Data.Map.Map String RewriteRule

-- | Creates an empty RelDict.
empty :: RelDict
empty = Data.Map.empty

-- | Returns true if a relation is already recorded.
hasRel :: RelDict -> String -> Bool
hasRel dict id = Data.Map.member id dict

-- | Records a identifier/relation pair inside a relation dictionary.
addRel :: RelDict -> (String, RewriteRule) -> RelDict
addRel dict (id, rel) = Data.Map.insert id rel dict

-- | Returns a relation by its name. If the relation does not exist, then nothing is
-- returned. To check if a generator is recorded, then use (hasRel dict id).
interpretRel :: RelDict -> String -> Maybe RewriteRule
interpretRel dict id = Data.Map.lookup id dict
