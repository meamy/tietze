-- | This module provides data types to store derivation files, and functions to analyze
-- the dependencies of derivation files.

module Lafont.Rewrite.Derivations (
    -- Re-exports from internal.
    Derivation ( .. ),
    DerivationMetadata,
    -- Exports.
    concretizeDerivation
) where

import           Lafont.Either
import           Lafont.Rewrite.Abstraction
import           Lafont.Rewrite.Internal.Derivations

-----------------------------------------------------------------------------------------
-- * Concretization of Abstract Derivations.

-- | Consumes a derivation map (dmap), an equational map (emap), and an abstract
-- derivation (deriv). If the abstract rewrites of deriv can be concretized, then a
-- concrete derivation is returned with the concretized rewrites. Otherwise, the line
-- number of first failing concretization is returned.
concretizeDerivation :: DerivationMetadata -> AbsDerivation -> Either Int Derivation
concretizeDerivation meta (AbsDerivation summary absRewrites) =
    updateRight (concretizeRewrites 0 meta absRewrites) (Derivation summary)
