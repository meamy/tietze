-- | General-purpose Ring utilities not found in newsynth.

module Lafont.Ring where

import Quantum.Synthesis.Ring

-----------------------------------------------------------------------------------------
-- Dyadic Value Manipulation.

-- | Consume a dyadic integer (Dyadic a n). Returns a new dyadic integer (Dyadic a' n')
-- such that (Dyadic a n) == (Dyadic a' n') with n' minimal.
reduceDyadic :: Dyadic -> Dyadic
reduceDyadic (Dyadic a 0) = Dyadic a 0
reduceDyadic (Dyadic a n) = if even a
                            then reduceDyadic (Dyadic (a `div` 2) (n - 1))
                            else Dyadic a n
