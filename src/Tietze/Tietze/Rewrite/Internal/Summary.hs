-- | Internals for Summary. Enables unit testing.

module Tietze.Rewrite.Internal.Summary
  ( EqMap (..)
  , DRuleSet (..)
  ) where
 
-----------------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.Map as Map
import qualified Data.Set as Set

-----------------------------------------------------------------------------------------
-- * Functions to Abstract Derivation Summaries as Rules.

-- | A mapping from derivation names to their equatioanl flags. If the equational flag of
-- a derived relation is true, then the derived relation may be applied in any direction.
newtype EqMap = EqMap (Map.Map String Bool) deriving (Eq,Show)

-----------------------------------------------------------------------------------------
-- * Functions to Register Derivations as Relations.

-- | A collection of unique derived relation symbols.
newtype DRuleSet = DRuleSet (Set.Set String) deriving (Eq,Show)
