-- | This module provides data types to store derivation files, and functions to analyze
-- the dependencies of derivation files.

module Lafont.Rewrite.Derivations where

import           Lafont.Rewrite.Rules
import           Lafont.Rewrite.Summary

-----------------------------------------------------------------------------------------
-- * Types to Represent a Derivation

-- | A concrete description of a derivation (i.e., includes all rewrite data).
data Derivation = Derivation DerivationSummary [Rewrite] deriving (Eq,Show)
