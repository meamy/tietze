-- | This module provides data types to store derivation files, and functions to analyze
-- the dependencies of derivation files.

module Tietze.Rewrite.Derivations
  ( Derivation (..)
  , DerivationMetadata
  , concretizeDerivation
  ) where
 
-----------------------------------------------------------------------------------------
-- * Import Section.

import Tietze.Either (updateRight)
import Tietze.Rewrite.Abstraction (AbsDerivation (..))
import Tietze.Rewrite.Internal.Derivations
  ( Derivation (..)
  , DerivationMetadata
  , concretizeRewrites
  )

-----------------------------------------------------------------------------------------
-- * Concretization of Abstract Derivations.

-- | Consumes a derivation map (dmap), an equational map (emap), and an abstract
-- derivation (deriv). If the abstract rewrites of deriv can be concretized, then a
-- concrete derivation is returned with the concretized rewrites. Otherwise, the line
-- number of first failing concretization is returned.
concretizeDerivation :: DerivationMetadata -> AbsDerivation -> Either Int Derivation
concretizeDerivation meta (AbsDerivation summary absRewrites) =
    updateRight (concretizeRewrites 0 meta absRewrites) (Derivation summary)
