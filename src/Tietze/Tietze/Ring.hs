-- | General-purpose Ring utilities not found in newsynth.

module Tietze.Ring (reduceDyadic) where
 
-------------------------------------------------------------------------------
-- * Import Section.

import qualified Quantum.Synthesis.Ring as QRing

-----------------------------------------------------------------------------------------
-- Dyadic Value Manipulation.

-- | Consume a dyadic integer (Dyadic a n). Returns a new dyadic integer (Dyadic a' n')
-- such that (Dyadic a n) == (Dyadic a' n') with n' minimal.
reduceDyadic :: QRing.Dyadic -> QRing.Dyadic
reduceDyadic (QRing.Dyadic a 0) = QRing.Dyadic a 0
reduceDyadic (QRing.Dyadic a n)
    | even a    = reduceDyadic (QRing.Dyadic (a `div` 2) (n - 1))
    | otherwise = QRing.Dyadic a n
