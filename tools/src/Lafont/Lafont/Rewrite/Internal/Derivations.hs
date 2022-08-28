-- | Internals for Derivations. Enables unit testing.

module Lafont.Rewrite.Internal.Derivations (
    Derivation ( .. ),
    DerivationMetadata,
    concretizeApply,
    concretizeRewrite,
    concretizeRewrites
) where

import           Lafont.Either
import           Lafont.Maybe
import           Lafont.Rewrite.Abstraction
import           Lafont.Rewrite.Common
import           Lafont.Rewrite.Rules
import           Lafont.Rewrite.Summary

-----------------------------------------------------------------------------------------
-- * Types to Represent a Derivation

-- | A concrete description of a derivation (i.e., includes all rewrite data).
data Derivation = Derivation DerivationSummary [Rewrite] deriving (Eq,Show)

-----------------------------------------------------------------------------------------
-- * Concretization of Abstract Derivations.

-- | Utility type to pass along a dmap/emap pair.
type DerivationMetadata = (DerivationMap, EqMap)

-- | Consumes an equational map (emap), derivation summary (sum), and derivation
-- application (Apply name pos dir). Computes the rule to abstract sum with respect to
-- emap (rule). If dir respects the equationality of rule, then (Rewrite rule pos dir) is
-- returned. Otherwise, nothing is returned.
concretizeApply :: EqMap -> DerivationSummary -> Apply -> Maybe Rewrite
concretizeApply emap sum (Apply name pos dir) = if dir == L2R || equational rule
                                                then Just (Rewrite rule pos dir)
                                                else Nothing
    where rule = createSummaryRule emap sum

-- | Consumes a derivation map (dmap), an equational map (emap), and an abstract rewrite.
-- If the abstract rewrite encodes a Rewrite, then the underlying Rewrite is returned. If
-- the abstract rewrite is an Apply, and the Apply can be concretized with respect to
-- to emap and the rule entry in dmap, then the concretized apply is returned. Otherwise,
-- nothing is returned.
concretizeRewrite :: DerivationMetadata -> AbsRewrite -> Maybe Rewrite
concretizeRewrite _            (Left  rw) = Just rw
concretizeRewrite (dmap, emap) (Right ap) =
    branchJust (dmap `getDerivation` name)
        (\(AbsDerivation sum _) -> concretizeApply emap sum ap)
    where (Apply name _ _) = ap

-- | Consumes a line number (num), a derivation map (dmap), an equational map (emap), and
-- a list of abstract rewrites. If all abstract rewrites can be concretized with respect
-- to dmap and emap, then the corresponding list of concrete rewrites are returned.
-- Otherwise, the line number of the first failing concretization is returned.
concretizeRewrites :: Int -> DerivationMetadata -> [AbsRewrite] -> Either Int [Rewrite]
concretizeRewrites num _    []                       = Right []
concretizeRewrites num meta (absRewrite:absRewrites) =
    case concretizeRewrite meta absRewrite of
        Nothing  -> Left num
        Just crw -> updateRight (concretizeRewrites (num + 1) meta absRewrites) (crw :)
