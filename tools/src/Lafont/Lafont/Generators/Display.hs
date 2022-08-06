-- | Provides the typing data required to display generators. Since most generators are
-- built on top of existing types, the Display class data must be provided separately.

module Lafont.Generators.Display where

import Quantum.Synthesis.Ring
import Quantum.Synthesis.Matrix
import Lafont.Common
import Lafont.Ring
import Lafont.Generators.QubitGates

-----------------------------------------------------------------------------------------
-- * Monoid Semantics.

instance Display () where
    display _ = "()"

-----------------------------------------------------------------------------------------
-- * Clifford(D)+Tof Semantics.

instance Display Dyadic where
    display d = case reduceDyadic d of
        (Dyadic a 0) -> show a
        (Dyadic 0 n) -> "0"
        (Dyadic a 1) -> show a ++ "/2"
        (Dyadic a n) -> show a ++ "/2^" ++ show n

displayRow :: (Display a) => [a] -> String
displayRow []      = ""
displayRow [a]     = display a
displayRow (a:row) = display a ++ ", " ++ displayRow row

displayRows :: (Display a) => [[a]] -> String
displayRows []       = ""
displayRows [r]      = "[" ++ displayRow r ++ "]"
displayRows (r:rows) = "[" ++ displayRow r ++ "], " ++ displayRows rows

instance (Nat n, Nat m, Display a) => Display (Matrix n m a) where
    display mat = "[" ++ displayRows (rows_of_matrix mat) ++ "]"
