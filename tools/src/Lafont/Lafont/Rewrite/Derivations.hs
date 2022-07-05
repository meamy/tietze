-- | This module provides data types to store derivation files, and functions to analyze
-- the dependencies of derivation files.

module Lafont.Rewrite.Derivations where

import qualified Data.Set
import qualified Data.Map
import Data.Maybe
import Lafont.Maybe
import Lafont.Rewrite.Rules
import Lafont.Rewrite.Summary

-----------------------------------------------------------------------------------------
-- * Types to Represent a Derivation

-- | A concrete description of a derivation (i.e., includes all rewrite data).
data Derivation = Derivation { summary :: DerivationSummary
                             , rewrites :: [Rewrite]
                             } deriving (Eq,Show)
